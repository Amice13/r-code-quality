
models_data <- 
  estimation_data %>%
  filter(!is.na(treatment)) %>%
  filter(treatment_observed == 0) %>%
  filter(Y6 == "Hampir semuanya dapat dipahami dengan baik") %>%
  mutate_at(vars(V_21_A, V_21_B, V_21_C, V_21_D, V_21_E, V_21_F),
            funs(case_when(. == "Sangat penting" ~ 4,
                           . == "Penting" ~ 3,
                           . %in% c("Tidak Penting", "Tidak penting")  ~ 2,
                           . == "Sangat tidak penting" ~ 1,
                           TRUE ~ NA_real_))) %>%
  mutate(pro_sociality = sum(V_21_A, V_21_B, V_21_C, V_21_D, V_21_E, V_21_F, na.rm = T)/6)

#here are the base models
test_mod1 <- lm(V_12_A== 4 ~ treatment*pro_sociality, data = models_data)
test_mod2 <- lm(V_12_B== 4 ~ treatment*pro_sociality, data = models_data)
test_mod3 <- lm(V_12_C== 4 ~ treatment*pro_sociality, data = models_data)
test_mod4 <- lm(V_12_D== 4 ~ treatment*pro_sociality, data = models_data)
test_mod5 <- lm(V_12_E== 4 ~ treatment*pro_sociality, data = models_data)
test_mod6 <- lm(V_12_F== 4 ~ treatment*pro_sociality, data = models_data)
test_mod7 <- lm(V_12_G== 4 ~ treatment*pro_sociality, data = models_data)
test_mod8 <- lm(avg_index_v12 ~ treatment*pro_sociality, data = models_data)

#extracting the observations
observations <- c(nobs(test_mod1),nobs(test_mod2),nobs(test_mod3),nobs(test_mod4),nobs(test_mod5),nobs(test_mod6), nobs(test_mod7), nobs(test_mod8))

#adding robust standard errors

test_mod1 <- coeftest(test_mod1, vcov=vcovHC(test_mod1,type="HC0") ) #?
test_mod2 <- coeftest(test_mod2, vcov=vcovHC(test_mod2,type="HC0") )
test_mod3 <- coeftest(test_mod3, vcov=vcovHC(test_mod3,type="HC0") )
test_mod4 <- coeftest(test_mod4, vcov=vcovHC(test_mod4,type="HC0") )
test_mod5 <- coeftest(test_mod5, vcov=vcovHC(test_mod5,type="HC0") )
test_mod6 <- coeftest(test_mod6, vcov=vcovHC(test_mod6,type="HC0") )
test_mod7 <- coeftest(test_mod7, vcov=vcovHC(test_mod7,type="HC0") )
test_mod8 <- coeftest(test_mod8, vcov=vcovHC(test_mod8,type="HC0") )

#putting models in a list
table <- list(test_mod1, test_mod2, test_mod3, test_mod4, test_mod5, test_mod6, test_mod7, test_mod8)

#writing the sub note
note_text <- paste(" Standard errors were calculated using the Huber-White (HC0) correction. 
                   Outcomes capturing whether respondents thought the following issues were very important: 
  (1) health, (2) education, (3) civil rights, (4) economy, (5) minority rights, (6) climate change, (7) pollution  (8) environment (index).")

#creating the stargazer table
table = stargazer(table, type = 'latex', 
                  title = "The Effect of Informational Treatment by Pro-sociality (First-order Beliefs)",
                  label = 'tab:testing_effect_main',
                  model.names = F,
                  model.numbers = T,
                  digits = 3,
                  column.separate = c(5, 3),
                  column.labels = c("Non-Environmental Issues:", "Environmental Issues:"),
                  
                  multicolumn = F,
                  dep.var.labels = NULL, 
                  add.lines = list(c("Observations", observations)),
                  covariate.labels = c("Treatment", "Pro-sociality Index ($Z$)", "Treatment X $Z$"),
                  #star.cutoffs = c(0.05, 0.01),
                  #float.env = 'sidewaystable',
                  keep.stat = c("n"),
                  notes = NULL,
                  notes.align = 'l')


#amending the stargazer output, appending note, printing, and rescaling (to 0.8 size)
#note that the amending stargazer output is removing certain lines from the standard output, such as
#the output has a line for "dependent variable", as well as lines breaks in between each coefficient, which we want to remove
#it also removes the **<0.05 line, etc, because this is contained in the new "note_text" object
table[-c(10, 11, 12, 18, 21, 24, 27, 32)] %>%
  append(., c("\\cline{2-6} \\cline{7-9}",
              " &  &  &  &  &  &  & \\\\",
              " & Health & Education & Civ. Rights & Economy & Min. Rights & Clim. Change & Pollution & Env. Index \\\\"), after = 11) %>%
  write_latex(., note_text, './_4_outputs/tables/table_a25.tex', .8)















