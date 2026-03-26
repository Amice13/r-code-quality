# ================================================================================
# REPLICATION CODE
# ================================================================================
# Article Title: "Investment Without Return? Individual Out-of-State Contributions 
# to US Direct Democracy Campaigns"
# Author: Madison Schroder

# ================================================================================
# SETUP AND LIBRARIES
# ================================================================================

install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
      library(package, character.only = TRUE)
    }
  }
}

# Define required packages
required_packages <- c(
  "dplyr",        
  "tidyr",        
  "scales",     
  "ggplot2",      
  "ggforce",      
  "patchwork",    
  "usmap",       
  "knitr",        
  "kableExtra",   
  "huxtable",     
  "lme4",        
  "sandwich",     
  "lmtest",      
  "texreg",       
  "fixest",       
  "caret"  
)

# Install and load all required packages
install_and_load(required_packages)

# Verify all packages loaded successfully
cat("All required packages loaded successfully!\n")
# ================================================================================
# DATA LOADING
# ================================================================================
#Set working directory to wherever data and code was saved
#For example (uncomment to use): setwd("~/Desktop/replication_data_and_analyses")
#Load datasets
load("state_bm_contributions.RData")
load("out_of_state_data.RData")
load("out_of_state_data_ind.RData")
load("DD_threshold_tracker.RData")
load("subjects_df.RData")
load("state_bm_contributions_controls.RData")

#See README.txt for dataset descriptions
# ================================================================================
# DATA PREPROCESSING
# ================================================================================

# Define valid direct democracy states
# These 26 states allow citizen-initiated ballot measures (initiatives/referenda)
# Excludes mis-coded contributions, and states with only legislative referrals 
#or no citizen-initiated direct democracy mechanisms

valid_states <- c("AK", "AZ", "AR", "CA", "CO", "FL", "ID", "IL",  "ME", "MD", 
                  "MA", "MI", "MS", "MO", "MT", "NE", "NV", "NM", "ND", "OH", 
                  "OK", "OR", "SD", "UT", "WA", "WY")

#filter main dataset by state and year, see Data and Methods for 
## year filtering explanation 
state_bm_contributions <- state_bm_contributions %>%
  filter(bm_state %in% valid_states, year >= 2006, year <= 2022)

# Filter all datasets to include only valid direct democracy states
out_of_state_data <- out_of_state_data %>%
  filter(bm_state %in% valid_states)

out_of_state_data_ind <- out_of_state_data_ind %>%
  filter(bm_state %in% valid_states)


# ================================================================================
# TABLE 1: SUMMARY STATISTICS
# ================================================================================

# Calculate basic descriptive statistics for the dataset
# Number of unique direct democracy measures
unique_measures <- state_bm_contributions %>%
  distinct(bm_state, ballot_measure) %>%
  nrow()

# Total contributions (in billions)
total_contributions <- sum(state_bm_contributions$amount, na.rm = TRUE) / 1e9

# Total out-of-state contributions (in billions)
out_of_state_total <- sum(state_bm_contributions$amount[state_bm_contributions$bm_state != state_bm_contributions$state], na.rm = TRUE) / 1e9

# Percentage of out-of-state contributions
pct_out_of_state <- (out_of_state_total / total_contributions) * 100


# Calculate median and mean donation sizes
median_donation <- median(state_bm_contributions$amount, na.rm = TRUE)
mean_donation <- mean(state_bm_contributions$amount, na.rm = TRUE)
median_out_of_state <- median(state_bm_contributions$amount[state_bm_contributions$bm_state != state_bm_contributions$state], na.rm = TRUE)
mean_out_of_state <- mean(state_bm_contributions$amount[state_bm_contributions$bm_state != state_bm_contributions$state], na.rm = TRUE)

# Create a table of summary statistics
summary_table <- data.frame(
  Metric = c(
    "Number of Direct Democracy Measures",
    "Number of States",
    "Total Contributions (Billions USD)",
    "Out-of-State Contributions (Billions USD)",
    "Out-of-State as % of Total",
    "Median Donation Amount (USD)",
    "Mean Donation Amount (USD)",
    "Median Out-of-State Donation (USD)",
    "Mean Out-of-State Donation (USD)"
  ),
  Value = c(
    format(unique_measures, big.mark = ","),
    "26",
    sprintf("$%.2f", total_contributions),
    sprintf("$%.2f", out_of_state_total),
    sprintf("%.1f%%", pct_out_of_state),
    sprintf("$%.2f", median_donation),
    sprintf("$%.2f", mean_donation),
    sprintf("$%.2f", median_out_of_state),
    sprintf("$%.2f", mean_out_of_state)
  )
)

kable(summary_table, 
      caption = "Table 1: Summary Statistics of Direct Democracy Contributions",
      booktabs = TRUE) %>%
  kable_styling(full_width = FALSE, font_size = 8, 
                latex_options = c("hold_position", "scale_down"))
print(summary_table)
# ================================================================================
# FIGURE 1: OVERALL TRENDS OF OUT-OF-STATE CONTRIBUTIONS
# ================================================================================

# Suppress dplyr summarise messages for cleaner output
options(dplyr.summarise.inform = FALSE)

# Filter data for out-of-state contributions only
# Time period: 2006-2022 (study period)
out_of_state_data <- state_bm_contributions %>%
  filter(bm_state != state, year >= 2006, year <= 2022)

# Create combined year labels for election cycles
# Groups years into 2-year cycles to account for election timing effects
# Even years are primary election years, odd years are off-cycle
out_of_state_data$combined_year <- ifelse(out_of_state_data$year %% 2 == 0, 
                                          paste0("(", substr(out_of_state_data$year, 3, 4), "-", substr(out_of_state_data$year + 1, 3, 4), ")"),
                                          paste0("(", substr(out_of_state_data$year - 1, 3, 4), "-", substr(out_of_state_data$year, 3, 4), ")"))

# Aggregate contributions by election cycle
overall_trend <- out_of_state_data %>%
  group_by(combined_year) %>%
  summarize(total_amount = sum(amount))

# Create trend plot showing growth over time
overall_trend <- ggplot(overall_trend, aes(x = factor(combined_year), y = total_amount)) +
  geom_line(group = 1) +  # Connect points with line
  geom_point(size = 1) +  # Add points at each cycle
  labs(title = "Figure 1: Overall Trends of Out-of-State Contributions",
       x = "Year", 
       y = "Total Amount") +
  # Format y-axis in millions for readability
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, prefix = "$", suffix = "M")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(hjust = 0, size = 6),
    panel.grid.minor = element_line(color = "gray", linetype = "dashed", size = 0.1),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
  )

print(overall_trend)
# ================================================================================
# FIGURE 2: CONTRIBUTIONS BY DONOR TYPE
# ================================================================================

# Filter for in-state contributions during even years only
# Even years chosen to match electoral cycles and reduce noise
in_state_data <- state_bm_contributions %>%
  filter(bm_state == state, year %% 2 == 0, year >= 2006, year <= 2022)

# Aggregate all donors by contribution type and year
# This creates the "All Donors" panel
all_donors_data <- state_bm_contributions %>%
  filter(year %% 2 == 0, year >= 2006, year <= 2022) %>%
  group_by(year, contribution_type = ifelse(bm_state == state, "In-State", "Out-of-State")) %>%
  summarize(total_amount = sum(amount))

# Filter for individual donors only
# EntityType field distinguishes individuals from organizations/corporations
individual_data <- state_bm_contributions %>%
  filter(EntityType == "Individual", year %% 2 == 0, year >= 2006, year <= 2022) %>%
  group_by(year, contribution_type = ifelse(bm_state == state, "In-State", "Out-of-State")) %>%
  summarize(total_amount = sum(amount))

# Filter for non-individual donors (organizations, corporations, unions, etc.)
non_individual_data <- state_bm_contributions %>%
  filter(EntityType != "Individual", year %% 2 == 0, year >= 2006, year <= 2022) %>%
  group_by(year, contribution_type = ifelse(bm_state == state, "In-State", "Out-of-State")) %>%
  summarize(total_amount = sum(amount))

# Function to format monetary values on y-axis
# Converts large numbers to millions/billions for readability
format_money <- function(x) {
  ifelse(x < 1e9,
         paste0("$", formatC(x / 1e6), "M"),
         paste0("$", formatC(x / 1e9), "B"))
}

# Create three separate panels for the combined figure
# Each panel shows stacked bar charts comparing in-state vs out-of-state giving

# Panel 1: All Donors
p1_dt <- ggplot(all_donors_data, aes(x = factor(year), y = total_amount, fill = contribution_type)) +
  geom_bar(stat = "identity") +  # Stacked bar chart
  labs(title = "All Donors",
       x = "Year",
       y = "Total Amount",
       fill = "Contribution Type") +
  scale_fill_manual(values = c("gray30", "gray70")) +  # Distinguish in-state vs out-of-state
  scale_y_continuous(labels = format_money) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10), 
        axis.title.x = element_blank(),  # Remove x-axis title to avoid repetition
        axis.title.y = element_text(size = 14),
        legend.position = "bottom")

# Panel 2: Individual Donors Only
p2_dt <- ggplot(individual_data, aes(x = factor(year), y = total_amount, fill = contribution_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Individuals",
       x = "Year",
       y = "Total Amount",
       fill = "Contribution Type") +
  scale_fill_manual(values = c("gray30", "gray70")) +
  scale_y_continuous(labels = format_money) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10), 
        axis.title.y = element_blank(),  # Remove y-axis title for middle panel
        axis.title.x = element_text(size = 14),
        legend.position = "bottom")

# Panel 3: Non-Individual Donors (Organizations, Corporations, etc.)
p3_dt <- ggplot(non_individual_data, aes(x = factor(year), y = total_amount, fill = contribution_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Non-Individuals",
       x = "Year",
       y = "Total Amount",
       fill = "Contribution Type") +
  scale_fill_manual(values = c("gray30", "gray70")) +
  scale_y_continuous(labels = format_money) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
        axis.title.y = element_blank(),  # Remove y-axis title for right panel
        axis.title.x = element_blank(),  # Remove x-axis title for right panel
        legend.position = "bottom")

# Combine all three panels with shared legend at bottom
donor_type_plot <- (p1_dt + p2_dt + p3_dt) + 
  plot_layout(ncol = 3, widths = c(1, 1, 1.2), guides = "collect") + 
  plot_annotation(title = "Figure 2: Contributions by Donor Type", 
                  theme = theme(plot.title = element_text(hjust = 0.5))) &
  theme(legend.position = "bottom")

print(donor_type_plot)

# ================================================================================
# FIGURE 3: DISTRIBUTION BY CONTRIBUTION SIZE
# ================================================================================

# Focus on individual out-of-state donors for detailed size analysis
# Create contribution size categories to understand donation patterns
out_of_state_data_ind_size <- out_of_state_data_ind %>%
  mutate(
    # Define size categories based on common campaign finance thresholds
    # $100 threshold chosen as it represents small donor activity
    # Higher thresholds align with federal disclosure requirements
    size_category = case_when(
      amount <= 100 ~ "$100 or less",
      amount > 100 & amount <= 999 ~ "$101 - $999", 
      amount > 999 & amount <= 10000 ~ "$1,000 - $10,000",
      amount > 10000 & amount <= 99999 ~ "$10,001 - $99,999",
      amount >= 100000 ~ "$100,000 +"
    )
  )

# Calculate both count and dollar amount percentages
# This reveals the disconnect between number of donors and financial influence
contribution_summary <- out_of_state_data_ind_size %>%
  group_by(size_category) %>%
  summarise(
    count = n(),                    # Number of contributions
    total_amount = sum(amount)      # Total dollar amount
  ) %>%
  mutate(
    percent_contributions = count / sum(count) * 100,              # Percentage by count
    percent_amount = total_amount / sum(total_amount) * 100        # Percentage by dollar amount
  ) %>%
  arrange(desc(percent_contributions))

# Remove any missing categories and order logically
contribution_summary <- contribution_summary %>%
  filter(!is.na(size_category)) %>%
  mutate(size_category = factor(size_category, levels = c(
    "$100 or less",
    "$101 - $999", 
    "$1,000 - $10,000",
    "$10,001 - $99,999",
    "$100,000 +"
  )))

# Panel 1: Percentage of contributions by count
# Shows how many donors fall into each category
p1_size <- ggplot(contribution_summary, aes(x = size_category, y = percent_contributions)) +
  geom_bar(stat = "identity", fill = "#4682B4", width = 0.7) +
  labs(
    title = "Percentage of Total Contributions by Count",
    x = NULL,
    y = "Percent of Contributions"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 9),
    axis.title.y = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  coord_flip()  # Horizontal bars for better readability

# Panel 2: Percentage of total dollar amount
# Shows financial influence of each donor category
p2_size <- ggplot(contribution_summary, aes(x = size_category, y = percent_amount)) +
  geom_bar(stat = "identity", fill = "#CD5C5C", width = 0.7) +
  labs(
    title = "Percentage of Total Dollar Amount",
    x = NULL,
    y = "Percent of Total Amount"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 9),
    axis.title.y = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  coord_flip()

# Combine panels to show the dramatic difference between count and financial influence
final_plot_size <- p1_size + p2_size +
  plot_layout(ncol = 2, widths = c(1, 1)) +
  plot_annotation(
    title = "Figure 3: Distribution of Out-of-State Individual Contributions by Size",
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 11)
    )
  )

print(final_plot_size)

# ================================================================================
# FIGURE 4: CONTRIBUTIONS BY POLICY TYPE
# ================================================================================

# Analyze which policy areas attract out-of-state individual contributions
# Policy types are based on FollowTheMoney schema, aggregated into broader categories
policy_data <- out_of_state_data_ind %>%
  group_by(subject_narrowed) %>%
  summarise(total_amount = sum(amount, na.rm = TRUE)) %>%
  mutate(percentage = total_amount / sum(total_amount) * 100) %>%
  # Combine small categories into "Other" for readability
  # 5% threshold chosen to focus on most significant policy areas
  mutate(subject_narrowed = ifelse(percentage < 5, "Other", subject_narrowed)) %>%
  group_by(subject_narrowed) %>%
  summarise(percentage = sum(percentage)) %>%
  arrange(desc(percentage))

# Create horizontal bar chart showing policy type preferences
policy_type_chart <- ggplot(policy_data, aes(x = reorder(subject_narrowed, percentage), y = percentage, fill = subject_narrowed)) +
  geom_bar(stat = "identity") +
  # Add percentage labels for precise values
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +
  scale_fill_brewer() +  # Use ColorBrewer palette for distinct colors
  coord_flip() +
  labs(title = "Figure 4: Individual Out-of-State Contributions by Policy Type",
       x = NULL,
       y = "Percentage of Total Contributions") +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend as colors are not meaningful
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), 
                     limits = c(0, max(policy_data$percentage) * 1.1))
print(policy_type_chart)

# ================================================================================
# FIGURE 5: GEOGRAPHIC AND INCOME DISTRIBUTION
# ================================================================================

# Set maximum income for consistent plotting
max_income <- max(out_of_state_data_ind$income, na.rm = TRUE)

# Define all 50 US states for origin mapping
# Includes all states regardless of direct democracy status to show where donors come from
us_states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
               "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
               "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
               "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
               "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

# Aggregate out-of-state contributions by origin state
# This shows which states are the largest sources of out-of-state money
state_amount_origin <- out_of_state_data_ind %>%
  filter(state %in% us_states) %>%  # Include only valid US states
  group_by(state) %>%
  summarise(total_amount = sum(amount))

# Convert state column to character for mapping compatibility
state_amount_origin$state <- as.character(state_amount_origin$state)

# Create log transformation for better visualization of large range in contributions
state_amount_origin$log_total_amount <- log(state_amount_origin$total_amount)

# Panel A: Income distribution histogram
# Uses ZIP code median income as proxy for donor income
# Right-skewed distribution suggests many donors come from moderate-income areas
p1_income <- ggplot(out_of_state_data_ind, aes(x = income)) +
  geom_histogram(binwidth = 10000,  # $10,000 income bins
                 color = "white",   # White borders between bars
                 aes(y = stat(count / sum(count)))) +  # Convert to proportions
  labs(title = "Figure 5a: Donor Income Distribution", 
       x = "Income", 
       y = "Proportion of Donors") +
  # Format x-axis with dollar signs and reasonable break points
  scale_x_continuous(labels = scales::dollar, breaks = seq(0, max_income, by = 20000)) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate labels for readability
  )

# Panel B: Geographic distribution map
# Shows total contributions by state of origin using color intensity
p2_map <-plot_usmap(data = state_amount_origin, values = "total_amount", color = "white") +
  scale_fill_gradient(
    low = "yellow", high = "red",  # Yellow to red color scheme
    name = "Total Contributions\n(Log Scale)", 
    na.value = "grey90",  # Gray for states with no data
    trans = "log10",      # Log transformation for better visualization
    labels = scales::dollar_format(prefix = "$"),
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 15,
      barheight = 0.5
    )
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.margin = margin(t = 10, r = 0, b = 0, l = 0),
    legend.box.margin = margin(t = 10, r = 0, b = 0, l = 0)
  ) +
  labs(title = "Figure 5b: Contributions By Origin State")

# Combine both panels with appropriate sizing
income_map_plot <- (p1_income + p2_map) + 
  plot_layout(ncol = 2, widths = c(2, 2.4), guides = "keep") & 
  plot_annotation(title = "Figure 5: Contribution Characteristics: Origin and Income", 
                  theme = theme(plot.title = element_text(hjust = 0.5))) &
  theme(legend.position = "bottom") &
  theme(plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")) 

#click zoom here for better readability
print(income_map_plot)

# ================================================================================
# THEORY TESTING: IDEOLOGICAL MOTIVATIONS
# ================================================================================

# Test whether out-of-state donors exhibit ideologically consistent giving patterns
# Focus on donors who gave to multiple campaigns to assess consistency

ideological_patterns <- out_of_state_data_ind %>%
  group_by(contributor_name) %>%
  filter(n() > 1) %>%  # Only include donors with multiple contributions
  summarise(
    # Basic giving statistics
    num_donations = n(),
    num_states = n_distinct(bm_state),
    
    # Subject matter consistency
    # Measures whether donors focus on similar policy areas
    primary_subject = names(which.max(table(subject_narrowed))),
    subject_consistency = max(table(subject_narrowed)) / n(),
    
    # Position consistency (support vs. oppose)
    # Measures whether donors consistently support or oppose measures
    position_consistency = max(table(position)) / n(),
    
    # Focus on ideological issues
    # These are "values-based" rather than economic policy areas
    pct_ideological = mean(subject_narrowed %in% 
                             c("Social Issues", "LGBTQIA Rights & Issues", 
                               "Abortion", "Substance Use & Regulation")),
    
    total_amount = sum(amount)
  ) %>%
  filter(num_donations >= 2)  # Remove potential data entry duplicates

# Statistical tests for ideological consistency
consistency_tests <- list(
  # Test if subject consistency exceeds random chance (50%)
  subject_test = t.test(ideological_patterns$subject_consistency, mu = 0.5),
  
  # Test if position consistency exceeds random chance (50%)
  position_test = t.test(ideological_patterns$position_consistency, mu = 0.5),
  
  # Test correlation between consistency and donation amounts
  # Negative correlation might suggest larger donors are more focused
  amount_correlation = cor.test(
    ideological_patterns$subject_consistency,
    ideological_patterns$total_amount
  )
)

# Calculate summary statistics for Table 2
## NOTE: In original manuscript, Table 2 was omitted and table labels were not 
#updated sufficiently. These have since been updated. 
summary_stats <- ideological_patterns %>%
  summarise(
    total_repeat_donors = n(),
    avg_states = mean(num_states),
    mean_subject_consistency = mean(subject_consistency),
    mean_position_consistency = mean(position_consistency),
    pct_primarily_ideological = mean(pct_ideological > 0.5) * 100
  )

# Format Table 2: Multi-State Donor Characteristics
summary_table_ideological <- data.frame(
  Metric = c(
    "Total Multi-State Donors",
    "Average States per Donor", 
    "Subject Consistency Score",
    "Position Consistency Score",
    "% Primarily Ideological Donors"
  ),
  Value = c(
    format(summary_stats$total_repeat_donors, big.mark = ","),
    sprintf("%.1f", summary_stats$avg_states),
    sprintf("%.1f%%", summary_stats$mean_subject_consistency * 100),
    sprintf("%.1f%%", summary_stats$mean_position_consistency * 100),
    sprintf("%.1f%%", summary_stats$pct_primarily_ideological)
  )
)

kable(summary_table_ideological, 
      caption = "Table 2: Multi-State Donor Characteristics") %>%
  row_spec(0, bold = TRUE)

# Format Table 3: Statistical Test Results
test_table_ideology <- data.frame(
  Test = c(
    "Subject Consistency",
    "Position Consistency",
    "Amount-Consistency Correlation"
  ),
  Statistic = c(
    sprintf("t = %.2f", consistency_tests$subject_test$statistic),
    sprintf("t = %.2f", consistency_tests$position_test$statistic),
    sprintf("r = %.3f", consistency_tests$amount_correlation$estimate)
  ),
  "p-value" = c(
    ifelse(consistency_tests$subject_test$p.value < 0.001, 
           "< 0.001", 
           sprintf("%.3f", consistency_tests$subject_test$p.value)),
    ifelse(consistency_tests$position_test$p.value < 0.001, 
           "< 0.001", 
           sprintf("%.3f", consistency_tests$position_test$p.value)),
    ifelse(consistency_tests$amount_correlation$p.value < 0.001, 
           "< 0.001", 
           sprintf("%.3f", consistency_tests$amount_correlation$p.value))
  )
)

kable(test_table_ideology,
      caption = "Table 3: Statistical Test Results") %>%
  row_spec(0, bold = TRUE)

# FIGURE 6: SUBJECT CONSISTENCY HISTOGRAM
# ------------------------------------------------------------------------------
# histogram examines whether donors who focus on ideological causes
# show different patterns of subject matter consistency compared to 
# non-ideological donors.

consistency_hist <- ggplot(ideological_patterns, 
                           aes(x = subject_consistency, 
                               fill = pct_ideological > 0.5)) +
  geom_histogram(binwidth = 0.1,           # Each bin represents 10% consistency range
                 alpha = 0.8,              # Semi-transparent for overlapping bars
                 position = "identity",     # Overlay histograms rather than stack
                 color = "white") +        # White borders for visual separation
  scale_fill_manual(values = c("gray70", "steelblue"),
                    labels = c("Non-ideological Focus", "Ideological Focus"),
                    name = "Donor Type") +
  scale_x_continuous(labels = scales::percent_format(),
                     breaks = seq(0, 1, 0.2)) +  # 20% intervals on x-axis
  labs(title = "Subject Matter Consistency Among Multi-State Donors",
       subtitle = "Distribution of subject focus consistency across repeat donors",
       x = "Subject Consistency Ratio",
       y = "Number of Donors") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    aspect.ratio = 0.75
  )

print(consistency_hist)

# ------------------------------------------------------------------------------
# CONSUMPTION THEORY ANALYSIS: SMALL DONOR PATTERNS
# ------------------------------------------------------------------------------

# Filter and aggregate small donor data
# Small donors defined as those making donations of $100 or less
small_donor_patterns <- out_of_state_data_ind %>%
  filter(amount <= 100) %>%                    # Focus on small donations only
  group_by(contributor_name) %>%               # Aggregate by individual donor
  summarise(
    num_donations = n(),                       # Total number of donations made
    num_states = n_distinct(bm_state),        # Number of different states donated to
    total_amount = sum(amount),                # Total dollars donated
    .groups = 'drop'                          # Remove grouping for subsequent operations
  )

# STEP 2: Calculate summary statistics for small donors
summary_stats_small_donor <- small_donor_patterns %>%
  summarise(
    total_small_donors = n(),                          # Count of unique small donors
    avg_donations_per_donor = mean(num_donations),     # Mean donations per person
    avg_states_per_donor = mean(num_states),          # Mean states per person
    median_total_amount = median(total_amount)         # Median total giving
  )

# Create formatted summary table
summary_table_small_donor <- data.frame(
  Metric = c(
    "Total Small Donors",
    "Average Donations per Donor", 
    "Average States per Donor",
    "Median Total Amount"
  ),
  Value = c(
    format(summary_stats_small_donor$total_small_donors, big.mark = ","),
    round(summary_stats_small_donor$avg_donations_per_donor, 2),
    round(summary_stats_small_donor$avg_states_per_donor, 2),
    paste0("$", round(summary_stats_small_donor$median_total_amount, 2))
  )
)

# Display formatted table
kable(summary_table_small_donor, 
      caption = "Table 4: Summary Statistics for Small Donors",
      booktabs = TRUE) %>%
  kable_styling(font_size = 15,
                full_width = FALSE)

# ------------------------------------------------------------------------------
# STATISTICAL TEST: ONE-SAMPLE T-TEST FOR CONSUMPTION THEORY
# ------------------------------------------------------------------------------

# Suppress scientific notation for cleaner output
options(scipen = 999)

# Perform one-sample t-test
t_test_result_small_donor <- t.test(small_donor_patterns$num_donations, 
                                    mu = 1,                    # Test against mean of 1
                                    alternative = "greater")   # One-tailed test

# Calculate effect size (Cohen's d)
mean_donations <- mean(small_donor_patterns$num_donations)
sd_donations <- sd(small_donor_patterns$num_donations)
cohens_d <- (mean_donations - 1) / sd_donations

# Function to format p-values appropriately
format_pvalue <- function(p) {
  if (p < 0.001) {
    return("<0.001")
  } else {
    return(sprintf("%.3f", p))
  }
}

# Create results table
t_test_table_small_donor <- data.frame(
  "t_value" = round(t_test_result_small_donor$statistic, 2),
  "df" = round(t_test_result_small_donor$parameter, 0),
  "p_value" = format_pvalue(t_test_result_small_donor$p.value),
  "Mean" = round(mean_donations, 2),
  "Effect_size" = round(cohens_d, 2)
)

# Display results table
kable(t_test_table_small_donor, 
      caption = "Table 5: Statistical Test of Consumption Theory",
      col.names = c("t value", "df", "p value", "Mean", "Effect size (Cohen's d)"),
      booktabs = TRUE) %>%
  kable_styling(font_size = 15,
                full_width = FALSE)

#-------------------------------------------------------------------------------
# APPENDIX TABLES
#-------------------------------------------------------------------------------
########################
# TABLE A1: DISCLOSURE THRESHOLDS
########################
sorted_thresholds <- DD_threshold_tracker %>%
  arrange(State) %>%
  select("State", "Disclosure threshold")

kable(sorted_thresholds, 
      caption = "Table A1: Disclosure Thresholds for Direct Democracy Contributions by State",
      col.names = c("State", "Disclosure Threshold ($)"),
      align = c("l", "c"),
      booktabs = TRUE) %>%
  kable_styling(font_size = 5,
                full_width = FALSE)

options(knitr.kable.NA = '0')

########################
# TABLE B1: BALLOT MEASURES BY STATE AND YEAR
########################
ballot_measures_by_state_year <- state_bm_contributions %>%
  group_by(bm_state, year) %>%
  summarize(
    num_measures = n_distinct(ballot_measure)
  ) %>%
  ungroup() %>%
  arrange(bm_state, year)  # Ensure proper ordering

year_order <- sort(unique(ballot_measures_by_state_year$year))

ballot_measures_wide <- ballot_measures_by_state_year %>%
  pivot_wider(
    id_cols = bm_state,
    names_from = year,
    values_from = num_measures,
    values_fill = 0
  ) %>%
  select(bm_state, all_of(as.character(year_order))) %>%
  mutate(Total = rowSums(select(., -bm_state), na.rm = TRUE)) %>%
  rename(State = bm_state)

kable(ballot_measures_wide, 
      caption = "Table B1: Ballot Measures by State and Year",
      booktabs = TRUE,
      longtable = TRUE) %>%
  kable_styling(font_size = 4, 
                latex_options = c("repeat_header", "scale_down", "hold_position")) %>%
  column_spec(1, width = "0.6in") %>%  
  add_header_above(c(" " = 1, "Year" = ncol(ballot_measures_wide) - 2, " " = 1)) %>%
  landscape()

########################
# TABLE B2: TOTAL CONTRIBUTIONS BY STATE AND YEAR
########################
contributions_by_state_year <- state_bm_contributions %>%
  group_by(bm_state, year) %>%
  summarize(
    total_contributions = sum(amount, na.rm = TRUE) / 1e6  # Convert to millions
  ) %>%
  ungroup() %>%
  arrange(bm_state, year)  # Ensure proper ordering

# Get unique years in chronological order
year_order <- sort(unique(contributions_by_state_year$year))

# Reshape to wide format
contributions_wide <- contributions_by_state_year %>%
  pivot_wider(
    id_cols = bm_state,
    names_from = year,
    values_from = total_contributions,
    values_fill = 0
  ) %>%
  select(bm_state, all_of(as.character(year_order))) %>%
  mutate(Total = rowSums(select(., -bm_state), na.rm = TRUE)) %>%
  rename(State = bm_state)

kable(contributions_wide, 
      caption = "Table B2: Total Contributions by State and Year (Millions USD)",
      booktabs = TRUE,
      digits = 1,  
      longtable = TRUE) %>%
  kable_styling(font_size = 4, 
                latex_options = c("repeat_header", "scale_down", "hold_position")) %>%
  column_spec(1, width = "0.6in") %>%  
  add_header_above(c(" " = 1, "Year" = ncol(contributions_wide) - 2, " " = 1)) %>%
  landscape()

########################
# TABLE B3: OUT-OF-STATE CONTRIBUTIONS BY STATE AND YEAR
########################
out_of_state_by_state_year <- state_bm_contributions %>%
  filter(bm_state != state) %>%  
  group_by(bm_state, year) %>%
  summarize(
    out_of_state_contributions = sum(amount, na.rm = TRUE) / 1e6  # Convert to millions
  ) %>%
  ungroup() %>%
  arrange(bm_state, year)  

year_order <- sort(unique(out_of_state_by_state_year$year))

out_of_state_wide <- out_of_state_by_state_year %>%
  pivot_wider(
    id_cols = bm_state,
    names_from = year,
    values_from = out_of_state_contributions,
    values_fill = 0
  ) %>%
  select(bm_state, all_of(as.character(year_order))) %>%
  mutate(Total = rowSums(select(., -bm_state), na.rm = TRUE)) %>%
  rename(State = bm_state)

kable(out_of_state_wide, 
      caption = "Table B3: Out-of-State Contributions by State and Year (Millions USD)",
      booktabs = TRUE,
      digits = 1,  # Reduce decimal places to save space
      longtable = TRUE) %>%
  kable_styling(font_size = 5, 
                latex_options = c("repeat_header", "scale_down", "hold_position")) %>%
  column_spec(1, width = "0.8in") %>%  # Control state column width
  add_header_above(c(" " = 1, "Year" = ncol(out_of_state_wide) - 2, " " = 1)) %>%
  landscape()

########################
# TABLE B4: PERCENTAGE OF OUT-OF-STATE CONTRIBUTIONS BY STATE
########################
# Calculate the percentage of out-of-state contributions for each state
out_of_state_pct <- state_bm_contributions %>%
  group_by(bm_state) %>%
  summarize(
    total_contributions = sum(amount, na.rm = TRUE) / 1e6,  # Convert to millions directly
    out_of_state_contributions = sum(amount[bm_state != state], na.rm = TRUE) / 1e6,
    percentage = (out_of_state_contributions / total_contributions) * 100
  ) %>%
  arrange(desc(percentage)) %>%
  rename(State = bm_state)

kable(out_of_state_pct, 
      caption = "Table B4: Percentage of Out-of-State Contributions by State",
      col.names = c("State", "Total (M)", "Out-of-State (M)", "%"),  # Shorter column names
      booktabs = TRUE,
      digits = c(0, 1, 1, 1),  
      longtable = TRUE) %>%
  kable_styling(font_size = 5, 
                latex_options = c("repeat_header", "scale_down", "hold_position"))

########################
# TABLE B5: CONTRIBUTIONS BY STATE PAIRS (TOP 20)
########################
state_pairs <- state_bm_contributions %>%
  filter(bm_state != state) %>%  
  group_by(origin = state, destination = bm_state) %>%
  summarize(
    total_amount = sum(amount, na.rm = TRUE) / 1e6,  
    num_contributions = n(),
    num_measures = n_distinct(ballot_measure)
  ) %>%
  arrange(desc(total_amount)) %>%
  head(20)  # Top 20 state pairs

kable(state_pairs, 
      caption = "Table B5: Top 20 State Pairs by Out-of-State Contribution Amount",
      col.names = c("Origin", "Destination", "Total (M)", 
                    "# Contributions", "# Measures"),  
      booktabs = TRUE,
      digits = c(0, 0, 1, 0, 0),  
      longtable = TRUE) %>%
  kable_styling(font_size = 3.5, 
                latex_options = c("repeat_header", "scale_down", "hold_position")) %>%
  column_spec(1:2, width = "0.6in") 

########################
# TABLE B6: YEARLY TRENDS IN DIRECT DEMOCRACY FINANCING
########################
yearly_trends <- state_bm_contributions %>%
  group_by(year) %>%
  summarize(
    total_amount = sum(amount, na.rm = TRUE) / 1e6,  # Convert to millions
    out_of_state_amount = sum(amount[bm_state != state], na.rm = TRUE) / 1e6,
    pct_out_of_state = (out_of_state_amount / total_amount) * 100,
    num_measures = n_distinct(ballot_measure),
    num_contributions = n(),
    median_contribution = median(amount, na.rm = TRUE)
  ) %>%
  arrange(year)  

kable(yearly_trends, 
      caption = "Table B6: Yearly Trends in Direct Democracy Financing",
      col.names = c("Year", "Total (M)", "Out-of-State (M)", 
                    "% Out", "# Measures", "# Contributions", 
                    "Median ($)"),  
      booktabs = TRUE,
      digits = c(0, 1, 1, 1, 0, 0, 0),  
      longtable = TRUE) %>%
  kable_styling(font_size = 5, 
                latex_options = c("repeat_header", "scale_down", "hold_position"))

#####################################
# APPENDIX C1: POLICY TYPES TABLE
#####################################
num_rows <- ceiling(nrow(subjects_df) / 3)

subjects_table <- data.frame(
  Column1 = c(subjects_df$Subject[1:num_rows], rep(NA, num_rows * 3 - nrow(subjects_df))),
  Column2 = c(subjects_df$Subject[(num_rows + 1):(2 * num_rows)], rep(NA, num_rows * 3 - nrow(subjects_df))),
  Column3 = c(subjects_df$Subject[(2 * num_rows + 1):nrow(subjects_df)], rep(NA, num_rows * 3 - nrow(subjects_df)))
)

# Remove any completely empty rows
subjects_table <- subjects_table[rowSums(is.na(subjects_table)) != ncol(subjects_table), ]

# Create the kable table
kable(subjects_table, 
      format = "latex",
      col.names = c("Subjects", "Subjects", "Subjects"),
      caption = "Table C1: Categorization of Direct Democracy Subjects",
      align = c("l", "l", "l"),
      booktabs = TRUE,
      linesep = "") %>%
  kable_styling(font_size = 8,  # Increased font size
                latex_options = "scale_down")

#######################
# FIGURE C1: IN-STATE POLICY DISTRIBUTION FOR COMPARISON
#######################
in_state_policy_data <- state_bm_contributions_controls %>%
  filter(EntityType == "Individual") %>%
  group_by(subject_narrowed) %>%
  summarise(total_amount = sum(amount, na.rm = TRUE)) %>%
  mutate(percentage = total_amount / sum(total_amount) * 100) %>%
  mutate(subject_narrowed = ifelse(percentage < 5, "Other", subject_narrowed)) %>%
  group_by(subject_narrowed) %>%
  summarise(percentage = sum(percentage)) %>%
  arrange(desc(percentage))

ggplot(in_state_policy_data, aes(x = reorder(subject_narrowed, percentage), y = percentage, fill = subject_narrowed)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +
  scale_fill_brewer() +
  coord_flip() +
  labs(title = "Figure C1: Individual Out-of-State Contributions by Policy Type",
       x = NULL,
       y = "Percentage of Total Contributions") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), 
                     limits = c(0, max(policy_data$percentage) * 1.1))


# END OF CODE

