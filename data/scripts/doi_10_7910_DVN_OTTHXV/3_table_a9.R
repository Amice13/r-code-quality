models_data <- 
  estimation_data %>%
  filter(!is.na(treatment)) %>%
  filter(treatment_observed == 0)


#here are the base models
test_mod1 <- lm(V_12_A==4 ~ treatment, data = models_data)
test_mod2 <- lm(V_12_B==4 ~ treatment, data = models_data)
test_mod3 <- lm(V_12_C==4 ~ treatment, data = models_data)
test_mod4 <- lm(V_12_D==4 ~ treatment, data = models_data)
test_mod5 <- lm(V_12_E==4 ~ treatment, data = models_data)
test_mod6 <- lm(V_12_F==4 ~ treatment, data = models_data)
test_mod7 <- lm(V_12_G==4 ~ treatment, data = models_data)
test_mod8 <- lm(avg_index_v12 ~ treatment, data = models_data)

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
note_text <- paste(" Coefficients from OLS regression. Robust and unclustered standard errors.
  Outcomes capturing whether respondents thought the following issues were very important: 
  (1) health, (2) education, (3) civil rights, (4) economy, (5) minority rights, (6) climate change, (7) pollution  (8) environment (index).")

#creating the stargazer table
table = stargazer(table, type = 'latex', 
                  title = "The Effect of Intervention on Politicians' First-Order Views",
                  label = 'tab:table_1',
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
  write_latex(., note_text, './_4_outputs/tables/table_a9.tex', .8)


