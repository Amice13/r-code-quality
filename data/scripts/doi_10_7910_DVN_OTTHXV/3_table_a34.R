library(stargazer)
# "V_12_F" ~ "Climate Change"
# V_12_D" ~ "Pollution
# Load Data and generate dummies
models_data <- 
  estimation_data %>%
  filter(!is.na(treatment)) %>%
  filter(treatment_observed == 0) %>%
  filter(Y6 == "Hampir semuanya dapat dipahami dengan baik") %>%
  mutate(across(starts_with("V_12"), ~ ifelse(. == 4, 1, 0), .names = "{.col}_dummy"))


#here are the base models
interact <- models_data$V_12_F_dummy
test_mod1 <- lm(V_61_A == 4 ~ treatment*interact, data = models_data)
test_mod2 <- lm(V_61_B == 4 ~ treatment*interact, data = models_data)
test_mod3 <- lm(V_61_C== 4 ~ treatment*interact, data = models_data)
test_mod4 <- lm(V_61_D== 4 ~ treatment*interact, data = models_data)
test_mod5 <- lm(V_61_E== 4 ~ treatment*interact, data = models_data)
test_mod6 <- lm(V_62_A== 4 ~ treatment*interact, data = models_data)
test_mod7 <- lm(V_62_B== 4 ~ treatment*interact, data = models_data)

#extracting the observations
observations <- c(nobs(test_mod1),nobs(test_mod2),nobs(test_mod3),nobs(test_mod4),nobs(test_mod5),nobs(test_mod6), nobs(test_mod7))

#adding robust standard errors

test_mod1 <- coeftest(test_mod1, vcov=vcovHC(test_mod1,type="HC0"), cluster = "kabupaten")
test_mod2 <- coeftest(test_mod2, vcov=vcovHC(test_mod2,type="HC0"), cluster = "kabupaten")
test_mod3 <- coeftest(test_mod3, vcov=vcovHC(test_mod3,type="HC0"), cluster = "kabupaten")
test_mod4 <- coeftest(test_mod4, vcov=vcovHC(test_mod4,type="HC0"), cluster = "kabupaten")
test_mod5 <- coeftest(test_mod5, vcov=vcovHC(test_mod5,type="HC0"), cluster = "kabupaten")
test_mod6 <- coeftest(test_mod6, vcov=vcovHC(test_mod6,type="HC0"), cluster = "kabupaten")
test_mod7 <- coeftest(test_mod7, vcov=vcovHC(test_mod7,type="HC0"), cluster = "kabupaten")


#putting models in a list
table <- list(test_mod1, test_mod2, test_mod3, test_mod4, test_mod5, test_mod6, test_mod7)

#writing the sub note
note_text <- paste("Standard errors were calculated using the Huber-White (HC0) correction. 
                   The first five outcomes measure perceived urgency of action required on (1) extreme heat, (2) flooding, 
                   (3) rising sea levels, (4) deforestation, (5) air pollution. The final two columns measure support for (6) 
                   a carbon tax and (7) a deforestation ban. All outcomes are dichotomized to capture the most extreme response on the Likert-scale.")

#creating the stargazer table
table = stargazer(table, type = 'latex', 
                  title = "The Effect of Intervention on Policy Support, by Politician First-Order Beliefs",
                  label = 'tab:table_hte_fo',
                  model.names = F,
                  model.numbers = T,
                  digits = 3,
                  column.separate = c(5, 2),
                  column.labels = c("Does Issue Merit Policy Attention:", "Support For Policy:"),
                  
                  multicolumn = F,
                  dep.var.labels = NULL, 
                  add.lines = list(c("Observations", observations)),
                  covariate.labels = c("Treatment ($T$)", "Pol. First Order ($C$)", "$T$ X $C$"),
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
              " & Health & Education & Civ. Rights & Economy & Min. Rights & Clim. Change & Pollution \\\\"), after = 11) %>%
  write_latex(., note_text, './_4_outputs/tables/table_a34.tex', .8)


