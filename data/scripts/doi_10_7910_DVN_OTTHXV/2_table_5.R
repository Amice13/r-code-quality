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
test_mod1 <- lm(V_61_A == 4 ~ treatment * z_lhkpn, data = models_data)
test_mod2 <- lm(V_61_B == 4 ~ treatment * z_lhkpn, data = models_data)
test_mod3 <- lm(V_61_C == 4 ~ treatment * z_lhkpn, data = models_data)
test_mod4 <- lm(V_61_D == 4 ~ treatment * z_lhkpn, data = models_data)
test_mod5 <- lm(V_61_E == 4 ~ treatment * z_lhkpn, data = models_data)
test_mod6 <- lm(V_62_A == 4 ~ treatment * z_lhkpn, data = models_data)
test_mod7 <- lm(V_62_B == 4 ~ treatment * z_lhkpn, data = models_data)

# Extract the number of observations
observations <- c(nobs(test_mod1), nobs(test_mod2), nobs(test_mod3), nobs(test_mod4),
                  nobs(test_mod5), nobs(test_mod6), nobs(test_mod7))

# Add robust standard errors
robust_se <- function(model) {
  coeftest(model, vcov = vcovHC(model, type = "HC0"), cluster = "kabupaten")
}

test_mod1 <- robust_se(test_mod1)
test_mod2 <- robust_se(test_mod2)
test_mod3 <- robust_se(test_mod3)
test_mod4 <- robust_se(test_mod4)
test_mod5 <- robust_se(test_mod5)
test_mod6 <- robust_se(test_mod6)
test_mod7 <- robust_se(test_mod7)

# List of models
table <- list(test_mod1, test_mod2, test_mod3, test_mod4, test_mod5, test_mod6, test_mod7)

# Custom note text
note_text <- " Coefficients from OLS regression. Robust and clustered standard errors at the kabupaten level. The first five outcomes measure perceived urgency of action required on (1) extreme heat, (2) flooding, (3) rising sea levels, (4) deforestation, (5) air pollution. The final two columns measure support for (6) a carbon tax and (7) a deforestation ban. All outcomes are dichotomized to capture the most extreme response on the Likert-scale."

# Generate the stargazer table
table_latex = stargazer(table, type = 'latex',
                  title = "Effect of intervention on policy support, by level of clientelism",
                  label = 'tab:table_client',
                  model.names = FALSE,
                  model.numbers = TRUE,
                  digits = 3,
                  column.separate = c(5, 2),
                  column.labels = c("Does issue merit policy attention:", "Support for policy:"),
                  multicolumn = FALSE,
                  dep.var.labels = NULL,
                  add.lines = list(c("Observations", observations)),
                  covariate.labels = c("Treatment ($T$)", "Asset index ($Z$)", "$T$ X $Z$"),
                  keep.stat = c("n"),
                  notes = note_text,
                  notes.align = 'l',
                  star.char = c("\\dag", "*", "**"),
                  star.cutoffs = c(0.10, 0.05, 0.01),
                  notes.append = FALSE)

table_latex[-c(10, 11, 12, 18, 21, 24, 27, 32)] %>%
  append(., c("\\cline{2-6} \\cline{7-8}",
              " &  &  &  &  &  &  & \\\\",
              " & Extreme heat & Flooding & Sea level & Deforestation & Pollution & Carbon tax & Deforestation ban \\\\"), after = 11) %>%
  write_latex(., note_text, './_4_outputs/tables/table_5.tex', .8)

# Generate the stargazer table in HTML format (with default note suppressed)
table_html <- stargazer(table, type = 'html',
                        title = "Effect of intervention on policy support, by level of clientelism",
                        label = 'tab:table_client',
                        model.names = FALSE,
                        model.numbers = TRUE,
                        digits = 3,
                        column.separate = c(5, 2),
                        column.labels = c("Does issue merit policy attention:", "Support for policy:"),
                        multicolumn = FALSE,
                        dep.var.labels = NULL,
                        add.lines = list(c("Observations", observations)),
                        covariate.labels = c("Treatment (T)", "Asset index (Z)", "T X Z"),
                        keep.stat = c("n"),
                        notes = "",            # Suppress default note text
                        notes.align = 'l',
                        star.char = c("&dagger;", "*", "**"),
                        star.cutoffs = c(0.10, 0.05, 0.01),
                        notes.append = FALSE,
                        omit.table.layout = "n")  # Omit LaTeX-specific note section

# Remove italics from "Dependent variable:" if present
table_html <- gsub("<em>Dependent variable:</em>", "Dependent variable:", table_html)

# Write the HTML table (with the custom note appended at the bottom) to a file
write_html(table_html, note_text, './_4_outputs/tables_doc/table_5.html')

