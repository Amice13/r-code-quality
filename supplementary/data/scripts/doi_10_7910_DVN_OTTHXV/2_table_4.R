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
test_mod1 <- lm(V_61_A ~ treatment * clim_change_misperc, data = models_data)
test_mod2 <- lm(V_61_B ~ treatment * clim_change_misperc, data = models_data)
test_mod3 <- lm(V_61_C ~ treatment * clim_change_misperc, data = models_data)
test_mod4 <- lm(V_61_D ~ treatment * clim_change_misperc, data = models_data)
test_mod5 <- lm(V_61_E ~ treatment * clim_change_misperc, data = models_data)
test_mod6 <- lm(V_62_A ~ treatment * clim_change_misperc, data = models_data)
test_mod7 <- lm(V_62_B ~ treatment * clim_change_misperc, data = models_data)

# Extract the number of observations
observations <- c(nobs(test_mod1), nobs(test_mod2), nobs(test_mod3), nobs(test_mod4),
                  nobs(test_mod5), nobs(test_mod6), nobs(test_mod7))

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

# List of models
table <- list(test_mod1, test_mod2, test_mod3, test_mod4, test_mod5, test_mod6, test_mod7)

# Custom note text
note_text <- " Coefficients from OLS regression. Robust and unclustered standard errors. The first five outcomes measure perceived urgency of action required on (1) extreme heat, (2) flooding, (3) rising sea levels, (4) deforestation, (5) air pollution. The final two columns measure support for (6) a carbon tax and (7) a deforestation ban."

# Generate the stargazer table
table_latex = stargazer(table, type = 'latex',
                        title = "Effect of informational treatment by scale of misperception",
                        label = 'tab:effect_mispercept',
                        model.names = FALSE,
                        model.numbers = TRUE,
                        digits = 3,
                        column.separate = c(5, 2),
                        column.labels = c("Does issue merit policy attention:", "Support for policy:"),
                        multicolumn = FALSE,
                        dep.var.labels = NULL,
                        add.lines = list(c("Observations", observations)),
                        covariate.labels = c("Treatment ($T$)", "Misperception ($M$)", "$T$ X $M$"),
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
  write_latex(., note_text, './_4_outputs/tables/table_4.tex', .8)

# Generate the stargazer table in HTML format (with default note suppressed)
table_html <- stargazer(table, type = 'html',
                        title = "Effect of informational treatment by scale of misperception",
                        label = 'tab:effect_mispercept',
                        model.names = FALSE,
                        model.numbers = TRUE,
                        digits = 3,
                        column.separate = c(5, 2),
                        column.labels = c("Does issue merit policy attention:", "Support for policy:"),
                        multicolumn = FALSE,
                        dep.var.labels = NULL,
                        add.lines = list(c("Observations", observations)),
                        covariate.labels = c("Treatment (T)", "Misperception (M)", "T X M"),
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
write_html(table_html, note_text, './_4_outputs/tables_doc/table_4.html')

# Generate Mean value of prob_importance_climatechange
c <- sikap_df %>%
  select(prob_importance_climatechange) %>%
  mutate(prob_importance_climatechange = case_when(
    prob_importance_climatechange == "Sangat penting" ~ 4,
    prob_importance_climatechange == "Penting" ~ 3,
    prob_importance_climatechange == "Tidak penting" ~ 2,
    prob_importance_climatechange == "Sangat tidak penting" ~ 1
  )) %>%
  summarise(mean = mean(prob_importance_climatechange, na.rm=TRUE))

write_csv(c, './_4_outputs/supplementary/table_4.csv')

