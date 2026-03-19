# Load necessary libraries
library(dplyr)
library(lmtest)
library(sandwich)
library(stargazer)

# Filter the data
models_data <- estimation_data %>%
  filter(!is.na(treatment)) %>%
  filter(treatment_observed == 0) %>%
  filter(Y6 == "Hampir semuanya dapat dipahami dengan baik")

# Define the base models
test_mod1 <- lm(V_13_A == 4 ~ treatment, data = models_data)
test_mod2 <- lm(V_13_B == 4 ~ treatment, data = models_data)
test_mod3 <- lm(V_13_C == 4 ~ treatment, data = models_data)
test_mod4 <- lm(V_13_D == 4 ~ treatment, data = models_data)
test_mod5 <- lm(V_13_E == 4 ~ treatment, data = models_data)
test_mod6 <- lm(V_13_F == 4 ~ treatment, data = models_data)
test_mod7 <- lm(V_13_G == 4 ~ treatment, data = models_data)
test_mod8 <- lm(avg_index_v13 ~ treatment, data = models_data)

# Extract the number of observations
observations <- c(nobs(test_mod1), nobs(test_mod2), nobs(test_mod3), nobs(test_mod4),
                  nobs(test_mod5), nobs(test_mod6), nobs(test_mod7), nobs(test_mod8))

# Add robust standard errors
robust_se <- function(model) {
  coeftest(model, vcov = vcovHC(model, type = "HC0"))
}

test_mod1 <- robust_se(test_mod1)
test_mod2 <- robust_se(test_mod2)
test_mod3 <- robust_se(test_mod3)
test_mod4 <- robust_se(test_mod4)
test_mod5 <- robust_se(test_mod5)
test_mod6 <- robust_se(test_mod6)
test_mod7 <- robust_se(test_mod7)
test_mod8 <- robust_se(test_mod8)

# List of models (note the reordering)
table <- list(test_mod1, test_mod2, test_mod3, test_mod7, test_mod5, test_mod6, test_mod4, test_mod8)

# Custom note text
note_text <- " Coefficients from OLS regression. Robust and unclustered standard errors. Outcomes capturing whether respondents thought voters thought the following issues were very important: (1) health, (2) education, (3) civil rights, (4) economy, (5) minority rights, (6) climate change, (7) pollution, (8) environment (index)."

# Generate the stargazer table
table_latex = stargazer(table, type = 'latex',
                  title = "Effect of intervention on politicians' second-order views",
                  label = 'tab:table_2',
                  model.names = FALSE,
                  model.numbers = TRUE,
                  digits = 3,
                  column.separate = c(5, 3),
                  column.labels = c("Non-environmental issues", "Environmental issues"),
                  multicolumn = FALSE,
                  dep.var.labels = NULL,
                  add.lines = list(c("Observations", observations)),
                  covariate.labels = c("Treatment"),
                  keep.stat = c("n"),
                  notes = note_text,
                  notes.align = 'l',
                  star.char = c("\\dag", "*", "**"),
                  star.cutoffs = c(0.10, 0.05, 0.01),
                  notes.append = FALSE)

table_latex[-c(10, 11, 12, 18, 21, 26)] %>%
  append(., c("\\cline{2-6} \\cline{7-9}",
              " &  &  &  &  &  &  & \\\\",
              " & Health & Education & Civ. rights & Economy & Min. rights & Clim. change & Pollution & Env. index \\\\"), after = 11) %>%
  write_latex(., note_text, './_4_outputs/tables/table_2.tex', .8)



# Generate the stargazer table in HTML format (with notes disabled in stargazer)
table_html <- stargazer(table, type = 'html',
                        title = "Effect of intervention on politicians' second-order views",
                        label = 'tab:table_2',
                        model.names = FALSE,
                        model.numbers = TRUE,
                        digits = 3,
                        column.separate = c(5, 3),
                        column.labels = c("Non-environmental issues", "Environmental issues"),
                        multicolumn = FALSE,
                        dep.var.labels = NULL,
                        add.lines = list(c("Observations", observations)),
                        covariate.labels = c("Treatment"),
                        keep.stat = c("n"),
                        notes = "",
                        notes.align = 'l',
                        star.char = c("&dagger;", "*", "**"),
                        star.cutoffs = c(0.10, 0.05, 0.01),
                        notes.append = FALSE,
                        omit.table.layout = "n")
table_html <- gsub("<em>Dependent variable:</em>", "Dependent variable:", table_html)

# Write the HTML table (with the note appended at the bottom) to a file
write_html(table_html, note_text, './_4_outputs/tables_doc/table_2.html')

################################################

#Generate a csv with details of the regressions
model_names <- c("Health", "Education", "Civil rights", "Economy", 
                 "Minority rights", "Climate change", "Pollution", "Env index")

results_df <- lapply(seq_along(table), function(i) {
  m <- table[[i]]
  data.frame(
    outcome = model_names[i],
    estimate = m["treatment", "Estimate"],
    std_error = m["treatment", "Std. Error"],
    p_value = m["treatment", "Pr(>|t|)"],
    n_obs = observations[i]
  )
}) %>%
  bind_rows()

write_csv(results_df, "./_4_outputs/supplementary/table_2.csv")
