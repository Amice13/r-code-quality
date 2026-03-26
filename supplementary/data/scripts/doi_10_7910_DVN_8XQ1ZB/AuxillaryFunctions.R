## ---------------------------
##
## Script name: Auxillary Functions for the Replication and improvement code for Do Women Officers Police Differently? Evidence from Traffic Stops
##
## Purpose of script: Auxillary Functions
##
## Date of last modification: 2023-09-19
##
## Authors: Dianyi Yang
## ---------------------------
##
## Notes: Users do not need to manually run this code as it is sourced in Step 4.
## ---------------------------
bootstrap_prediction <- function(data, regression, num_simulations, seed) {
  # use a bootstrap sampling to make up for the bias in using mode values, and the computational costs of calculating weighted averages
  predicted_probs_male <- numeric(num_simulations) #create some new variables for simulations
  predicted_probs_female <- numeric(num_simulations)
  var_probs_male <- numeric(num_simulations) #variances of simulation results
  var_probs_female <- numeric(num_simulations)
  
  set.seed(seed)  # Set a seed for reproducibility
  
  for (i in 1:num_simulations) {
    # Randomly sample the values for the categorical variables with replacement
    sampled_data <- data[sample(nrow(data), 1),]
    
    # Set the value of of_gender for male and female officers
    sampled_data_male <- sampled_data
    sampled_data_female <- sampled_data
    sampled_data_male$of_gender <- 0
    sampled_data_female$of_gender <- 1
    
    # Predict the probabilities and standard errors for the sampled combination
    male_prediction <- predict(regression, newdata = sampled_data_male, type = "response", se.fit = TRUE) #so the same data are used, the only difference being officer sex
    female_prediction <- predict(regression, newdata = sampled_data_female, type = "response", se.fit = TRUE)
    predicted_probs_male[i] <- male_prediction$fit #store predicted values
    predicted_probs_female[i] <- female_prediction$fit
    var_probs_male[i] <- male_prediction$se.fit^2 #take the squared values for variacne
    var_probs_female[i] <- female_prediction$se.fit^2
  }
  
  # Compute the average of predicted probabilities and standard errors
  mean_predicted_prob_male <- mean(predicted_probs_male) #average predicted probability as the mean value
  mean_predicted_prob_female <- mean(predicted_probs_female)
  mean_se_male <- sqrt(mean(var_probs_male)) #hear I calculate the mean se as the square root of mean variance 
  mean_se_female <- sqrt(mean(var_probs_female)) #I'm not sure this is the best way to calculate se but I used it.
  
  # Construct the confidence intervals
  alpha <- 0.05  # Set the significance level for the confidence intervals, here it's 5%
  z <- qnorm(1 - alpha / 2)  # Compute the critical value for a two-tailed test
  
  # Compute the lower and upper bounds of the confidence intervals
  lower_bound_male <- mean_predicted_prob_male - z * mean_se_male
  upper_bound_male <- mean_predicted_prob_male + z * mean_se_male
  lower_bound_female <- mean_predicted_prob_female - z * mean_se_female
  upper_bound_female <- mean_predicted_prob_female + z * mean_se_female
  
  return(list(predicted_probs = list(male = mean_predicted_prob_male,
                                     female = mean_predicted_prob_female),
              lower_ci = list(male = lower_bound_male,
                              female = lower_bound_female),
              upper_ci = list(male = upper_bound_male,
                              female = upper_bound_female))) #return the results
}
bootstrap_prediction <- cmpfun(bootstrap_prediction) #compile to speed up loops

hausman <- function (fe_model, re_model) 
{
  get_coef <- function(model) if('lmerMod' %in% class(model)) {lme4::fixef(model)} else coef(model)
  fe_coef <- get_coef(fe_model)
  re_coef <- get_coef(re_model) #lme4::fixef(re_model)
  fe_vcov <- vcov(fe_model)
  re_vcov <- vcov(re_model)
  fe_names <- names(fe_coef)
  re_names <- names(re_coef)
  common_coef_names <- re_names[re_names %in% fe_names]
  coefs <- common_coef_names[!(common_coef_names %in% "(Intercept)")]
  betas <- fe_coef[coefs] - re_coef[coefs]
  vcovs <- fe_vcov[coefs, coefs] - re_vcov[coefs, coefs]
  z <- as.numeric(abs(t(betas) %*% solve(vcovs) %*% betas))
  df <- length(betas)
  p <- stats::pchisq(z, df, lower.tail = FALSE)
  stat <- z
  names(stat) <- "chi-square"
  parameter <- df
  names(parameter) <- "df"
  alpha = 0.05
  results <- list(statistic = stat, p.value = p, parameter = parameter, 
                  method = "Hausman Test", data.name = "hsb")
  class(results) <- "htest"
  varcorr_df <- as.data.frame(lme4::VarCorr(re_model))
  if (sum(!is.na(varcorr_df$var2)) > 0) {
    warning("Random slopes detected! Interpret with caution.\nSee ?mlmhelpr::de() for more information.")
  }
  message_text <- if (p < 0.05) {
    "\n\nResults are significantly different. \nThe multilevel model may not be suitable."
  }
  else {
    "\n\nResults are not significantly different. \nThe multilevel model may be more suitable."
  }
  message(message_text)
  return(results)
}
