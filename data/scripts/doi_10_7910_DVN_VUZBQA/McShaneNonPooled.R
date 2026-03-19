set.seed(123) # For reproducibility

# Generating samples
sample1 <- c(rep(1, 220), rep(0, 180)) # First sample
sample1 <- sample(sample1) # Randomizing
sample2 <- c(rep(1, 180), rep(0, 220)) # Second sample
sample2 <- sample(sample2) # Randomizing

# Calculating proportions for each sample
p1 <- mean(sample1)
p2 <- mean(sample2)

# Sample sizes
n1 <- length(sample1)
n2 <- length(sample2)

# Calculating variances for each sample proportion
var1 <- p1 * (1 - p1) / n1
var2 <- p2 * (1 - p2) / n2

# Test statistic for the non-pooled test
z <- (p1 - p2) / sqrt(var1 + var2)

# Displaying the z-score
cat("Z-score for the non-pooled test:", z, "\n")

# Determining the p-value
p_value <- 2 * pnorm(-abs(z)) # Two-tailed test

# Displaying the p-value
cat("P-value:", p_value, "\n\n")

# Necessary library
library(ggplot2)
library(scales)

# Running simulations to create the histogram
num_replications <- 100000 # Number of replications
difference_in_means <- numeric(num_replications) # Vector to store differences

for (i in 1:num_replications) {
  # Shuffling the combined sample
  shuffled_sample <- sample(c(sample1, sample2))
  
  # Splitting into two equal parts
  split1 <- shuffled_sample[1:400]
  split2 <- shuffled_sample[401:800]
  
  # Computing the difference in means
  difference_in_means[i] <- mean(split1) - mean(split2)
}

# Creating a dataframe from the vector
data <- data.frame(difference_in_means)

# Generating the histogram with a modified kernel density plot and larger axis text
ggplot(data, aes(x=difference_in_means)) +
  geom_histogram(aes(y=..density..), binwidth = 0.01, fill="lightblue", color="black", alpha=0.7) +
  geom_density(color="red", size=1.5, alpha=0) +
  geom_vline(xintercept = z, color="blue", size=1.5, linetype="solid") +
  labs(title="Histogram with Kernel Density Plot of Differences in Proportions",
       x="Difference in Proportions", y="Density") +
  scale_x_continuous(labels = scales::percent, limits = c(-0.15, 0.15)) + # Set x-axis limits
  theme(
    axis.text = element_text(size = rel(2)), # Doubling the size of axis text
    plot.title = element_text(hjust = 0.5), # Centering the title
    axis.title.y = element_text(size = rel(2)) # Y-axis label twice as large
  )



# Counting the number of times the absolute difference is greater than 0.1
num_times_exceeds <- sum(abs(difference_in_means) > 0.1)

# Displaying the result
num_times_exceeds
