set.seed(123) # Setting a seed for reproducibility

# Generating the first sample
sample1 <- c(rep(1, 220), rep(0, 180))
sample1 <- sample(sample1) # Randomizing

# Generating the second sample
sample2 <- c(rep(1, 180), rep(0, 220))
sample2 <- sample(sample2) # Randomizing

# Calculating proportions and standard deviations
proportion1 <- mean(sample1)
sd1 <- sqrt(proportion1*(1-proportion1))

proportion2 <- mean(sample2)
sd2 <- sqrt(proportion2*(1-proportion2))

# Displaying the results
cat("Sample 1 Proportion:", proportion1, "\nSample 1 SD:", sd1, "\n\nSample 2 proportion:", proportion2, "\nSample 2 SD:", sd2)


# Combining the first and second samples
combined_sample <- c(sample1, sample2)
combined_sample <- sample(combined_sample) # Randomizing the order

# Calculating the proportion and standard deviation of the combined sample
proportion_combined <- mean(combined_sample)
sd_combined <- sqrt(proportion_combined*(1-proportion_combined))

# Displaying the results
cat("Combined Sample Proportion:", proportion_combined, "\nCombined Sample SD:", sd_combined)



### Code for Figure 1

set.seed(123) # Setting a seed for reproducibility

num_replications <- 100000 # Number of replications
difference_in_proportions <- numeric(num_replications) # Vector to store differences
proportion_ones_split1 <- numeric(num_replications) # Vector to store proportion of ones in split1
proportion_ones_split2 <- numeric(num_replications) # Vector to store proportion of ones in split2

for (i in 1:num_replications) {
  # Shuffling the combined sample
  shuffled_sample <- sample(combined_sample)
  
  # Splitting into two equal parts
  split1 <- shuffled_sample[1:400]
  split2 <- shuffled_sample[401:800]
  
  # Computing the difference in proportions
  difference_in_proportions[i] <- mean(split1) - mean(split2)
  
  # Computing and storing the proportion of ones
  proportion_ones_split1[i] <- mean(split1)
  proportion_ones_split2[i] <- mean(split2)
}

# At this point, difference_in_proportions contains the differences for each replication,
# proportion_ones_split1 contains the proportion of ones in split1,
# and proportion_ones_split2 contains the proportion of ones in split2.



# Necessary library
library(ggplot2)
library(scales)

# Creating a dataframe from the vector
data <- data.frame(difference_in_proportions)

# Generating the histogram with a modified kernel density plot and larger axis text
ggplot(data, aes(x=difference_in_proportions)) +
  geom_histogram(aes(y=..density..), binwidth = 0.01, fill="lightblue", color="black", alpha=0.7) +
  geom_density(color="red", size=1.5, alpha=0) +
  geom_vline(xintercept = c(-0.10, 0.10), color="blue", size=1.5, linetype="solid") + # Blue vertical lines at -10% and 10%
  labs(title="Histogram with Kernel Density Plot of Differences in Proportions",
       x="Difference in Proportions", y="Density") +
  scale_x_continuous(labels = scales::percent, limits = c(-0.15, 0.15)) + # Set x-axis limits
  theme(
    axis.text = element_text(size = rel(2)), # Doubling the size of axis text
    plot.title = element_text(hjust = 0.5), # Centering the title
    axis.title.y = element_text(size = rel(2)) # Y-axis label twice as large
  )


# Counting the number of times the absolute difference is greater than 0.1
num_times_exceeds <- sum(abs(difference_in_proportions) > 0.1)

# Displaying the result
num_times_exceeds




### Code for Figure 2


set.seed(123) # Setting a seed for reproducibility

num_replications <- 100000 # Number of replications
proportion_ones_sample1 <- numeric(num_replications) # Vector to store proportions for sample1
proportion_ones_sample2 <- numeric(num_replications) # Vector to store proportions for sample2

for (i in 1:num_replications) {
  # Sampling with replacement from sample1 and sample2
  resampled_sample1 <- sample(sample1, size = 400, replace = TRUE)
  resampled_sample2 <- sample(sample2, size = 400, replace = TRUE)
  
  # Calculating and storing the proportion of ones
  proportion_ones_sample1[i] <- mean(resampled_sample1)
  proportion_ones_sample2[i] <- mean(resampled_sample2)
}

# At this point, proportion_ones_sample1 and proportion_ones_sample2 
# contain the proportions of ones for each sample across replications.





library(ggplot2)

# Calculating the specified quantities
difference <- proportion_ones_sample1 - proportion_ones_sample2
ratio <- proportion_ones_sample1 / proportion_ones_sample2
modified_ratio <- proportion_ones_sample2 / (proportion_ones_sample1^(3/2))

# Dataframes for each quantity
data_difference <- data.frame(difference)
data_ratio <- data.frame(ratio)
data_modified_ratio <- data.frame(modified_ratio)


library(ggplot2)
library(gridExtra) # Make sure this package is installed

# Assuming data_difference, data_ratio, and data_modified_ratio are already defined

# Plot for Difference
plot_difference <- ggplot(data_difference, aes(x=difference)) +
  geom_histogram(aes(y=..density..), bins=20, fill="lightblue", color="black", alpha=0.7) +
  geom_density(color="red", size=1.5, alpha=0) +
  geom_vline(xintercept=0, color="blue", size=1.5) +
  labs(title="Difference in Proportions",
       x="Difference", y="Density") +
  theme(axis.text = element_text(size = rel(2)),
        plot.title = element_text(size = rel(1.5), hjust = 0.5))  # Larger and centered title
plot_difference

# Plot for Ratio
plot_ratio <- ggplot(data_ratio, aes(x=ratio)) +
  geom_histogram(aes(y=..density..), bins=20, fill="lightblue", color="black", alpha=0.7) +
  geom_density(color="red", size=1.5, alpha=0) +
  geom_vline(xintercept=1.1, color="blue", size=1.5) +
  labs(title="Ratio of Proportions",
       x="Ratio", y="Density") +
  theme(axis.text = element_text(size = rel(2)),
        plot.title = element_text(size = rel(1.5), hjust = 0.5))  # Larger and centered title
plot_ratio

# Plot for Modified Ratio
plot_modified_ratio <- ggplot(data_modified_ratio, aes(x=modified_ratio)) +
  geom_histogram(aes(y=..density..), bins=20, fill="lightblue", color="black", alpha=0.7) +
  geom_density(color="red", size=1.5, alpha=0) +
  geom_vline(xintercept=1, color="blue", size=1.5) +
  labs(title="Proportion 2 / (Proportion 1)^(3/2)",
       x="Proportion 2 / (Proportion 1)^(3/2)", y="Density") +
  theme(axis.text = element_text(size = rel(2)),
        plot.title = element_text(size = rel(1.5), hjust = 0.5))  # Larger and centered title
plot_modified_ratio


grid.arrange(plot_difference, plot_ratio, plot_modified_ratio, ncol = 3)
