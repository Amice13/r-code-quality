#settings
library(readxl)
library(ggplot2)
library(dplyr)
library(nnet)
df <- read_excel("indices_deprec_top250_last_types 2024 09 24.xlsx", sheet = "dataframe")
df$change_percent <- as.numeric(as.character(df$change_percent))
df$date <- as.Date(as.character(df$date))
df$days <- as.numeric(df$date - min(df$date))
df$event_type <- as.factor(as.character(df$event_type))
df$event_type <- factor(df$event_type, levels = c("B", "A", "C", "D"))
class(df$change_percent)
class(df$date)
class(df$event_type)
class(df$days)
summary(df)

sink("econometric_output.txt")

#descriptive statistics - outputs

print("DESCRIPTIVE STATISTICS - OUTPUTS")

print("EVENT_TYPE")
table(df$event_type)

print("CHANGE_PERCENT")
summary(df$change_percent)

print("EXCHANGE")
table(df$exchange)

print("DATE")
table(df$date)


#regressoes - outputs

print("REGRESSIONS - OUTPUTS")

print("model1: CHANGE_PERCENT x EVENT_TYPE")
model1 <- lm(df$change_percent ~ df$event_type)
summary(model1)

print("model2: CHANGE_PERCENT x COUNTRY")
model2 <- lm(df$change_percent ~ df$country)
summary(model2)

print("model3: CHANGE_PERCENT x DAYS") 
model3 <- lm(df$change_percent ~ df$days)
summary(model3)

print("model4: EVENT_TYPE x COUNTRY")
model4 <- multinom(event_type ~ country, data = df)
summary(model4)
coeff_model4 <- summary(model4)$coefficients
std_err_model4 <- summary(model4)$standard.errors
z_model4 <- coeff_model4 / std_err_model4
z_model4
p_values_model4 <- 2 * (1 - pnorm(abs(z_model4)))
p_values_model4
odds_ratios_model4 <- exp(coef(model4))
odds_ratios_model4
probs_model4 <- predict(model4, type = "probs")
head(probs_model4)

print("model5: EVENT_TYPE x DAYS")  
model5 <- multinom(event_type ~ days, data = df)
summary(model5)
coeff_model5 <- summary(model5)$coefficients
std_err_model5 <- summary(model5)$standard.errors
z_model5 <- coeff_model5 / std_err_model5
z_model5
p_values_model5 <- 2 * (1 - pnorm(abs(z_model5)))
p_values_model5
odds_ratios_model5 <- exp(coef(model5))
odds_ratios_model5
probs_model5 <- predict(model5, type = "probs")
head(probs_model5)

sink()
