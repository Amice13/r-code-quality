load("McShaneNLPData.Rdata")
library(mgcv)


# Regressions
glm_NLP <- glm(formula = YesNo ~ NLPvar)
gam_NLP <- gam(YesNo ~ s(NLPvar), family = binomial)

summary(glm_NLP)
summary(gam_NLP)


# Graphics code

library(ggplot2)
library(mgcv)

# Prepare data for predictions
NLPvar_seq <- seq(min(NLPvar), max(NLPvar), length.out = 100)
prediction_data <- data.frame(NLPvar = NLPvar_seq)

# Predict probabilities for GLM
prediction_data$glm_prob <- predict(glm_NLP, newdata = prediction_data, type = "response")

# Predict probabilities and standard errors for GAM
gam_preds <- predict(gam_NLP, newdata = prediction_data, type = "response", se.fit = TRUE)
prediction_data$gam_prob <- gam_preds$fit
prediction_data$gam_se <- gam_preds$se.fit

# Calculate confidence intervals for GAM
ci_level <- 1.96 # 95% confidence interval
prediction_data$gam_upper <- prediction_data$gam_prob + ci_level * prediction_data$gam_se
prediction_data$gam_lower <- prediction_data$gam_prob - ci_level * prediction_data$gam_se

# Set Calibri font for labels
font <- "Calibri"

# Plot with enlarged axis text
ggplot(prediction_data, aes(x = NLPvar)) +
  geom_line(aes(y = glm_prob), color = "red", size = 1.2) + # Thicker GLM line
  geom_line(aes(y = gam_prob), color = "blue", size = 1.2) + # Thicker GAM line
  geom_ribbon(aes(ymin = gam_lower, ymax = gam_upper), alpha = 0.1, fill = "blue") + # Lighter contours
  geom_rug(data = data.frame(NLPvar = NLPvar), aes(x = NLPvar), sides = "b", color = "black", size = 1.2) + # Add rug plot based on NLPvar vector
  labs(x = "NLP Metric", y = "Predicted Probability", title = "GLM vs GAM Predictions, with Error Quantification") +
  theme_minimal() +
  theme(
    text = element_text(family = "Calibri"), # Set the font family to Calibri
    plot.title = element_text(size = 18, hjust = 0.5), # Adjust title size and center
    axis.title = element_text(size = 14), # Adjust axis label size
    axis.text.x = element_text(size = 12 * 2), # Enlarge X axis text
    axis.text.y = element_text(size = 12 * 2)  # Enlarge Y axis text
  )