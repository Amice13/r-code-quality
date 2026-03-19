library(rstan)

set.seed(123) # Setting a seed for reproducibility

# Generating the first sample
sample1 <- c(rep(1, 220), rep(0, 180))
sample1 <- sample(sample1) # Randomizing

# Generating the second sample
sample2 <- c(rep(1, 180), rep(0, 220))
sample2 <- sample(sample2) # Randomizing

# Data preparation
data_list <- list(
  N = length(sample1),
  y1 = sample1,
  y2 = sample2
)

# Stan model code
stan_model_code <- "
data {
  int<lower=0> N;       // Number of observations
  int<lower=0,upper=1> y1[N]; // Binary data for the first sample
  int<lower=0,upper=1> y2[N]; // Binary data for the second sample
}

parameters {
  real<lower=0,upper=1> p1; // Proportion for the first sample
  real<lower=0,upper=1> p2; // Proportion for the second sample
}

model {
  // Priors
  p1 ~ beta(1, 1); // Uniform prior for proportion 1
  p2 ~ beta(1, 1); // Uniform prior for proportion 2

  // Likelihood
  y1 ~ bernoulli(p1);
  y2 ~ bernoulli(p2);
}

generated quantities {
  real ratio = p2 / pow(p1, 1.5);
}
"

# Running the Stan model
fit <- stan(model_code = stan_model_code,  
            data = data_list,
            chains = 10,
            iter = 20000,  
            warmup = 10000,
            thin = 1)

# Extracting and printing results
print(fit, digits=3)



library(ggplot2)
library(rstan)

# Extracting the samples of the ratio from the model fit
posterior_samples <- extract(fit)$ratio

# Creating a dataframe for plotting
posterior_data <- data.frame(Ratio = posterior_samples)

# Plotting the posterior distribution
ggplot(posterior_data, aes(x = Ratio)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.01, fill = "lightblue", color = "black") +
  geom_density(color = "red", size = 1.5) +
  labs(title = "Posterior Distribution of (Proportion 2 / Proportion 1^(3/2))",
       x = "Ratio (Proportion 2 / Proportion 1^(3/2))",
       y = "Density") +
  theme_minimal() +
  theme(
    text = element_text(size = 20), # Doubling the text size
    plot.title = element_text(hjust = 0.5) # Centering the title
  )


# Extracting the samples of the ratio from the model fit
posterior_samples <- extract(fit)$ratio

# Calculating the proportion of samples less than 1
proportion_less_than_1 <- sum(posterior_samples < 1) / length(posterior_samples)

# Displaying the result
proportion_less_than_1
