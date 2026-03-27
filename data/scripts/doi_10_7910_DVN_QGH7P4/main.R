
# Call helper functions

source("helpers.R")

# Read in data

elite_df <- read.csv("../data/analysis/elite_survey.csv")
citizen_df <- read.csv("../data/analysis/citizen_survey.csv")
admin_df <- read.csv("../data/analysis/resignation_admin.csv")
experiment_df <- read.csv("../data/analysis/survey_experiment_clean.csv")

# Figure 2: Staggered electoral cycles in Maharashtra

elite_df$sarpanch_election_month_year_graph <- as.Date(paste(elite_df$sarpanch_election_year, elite_df$sarpanch_election_month, "1", sep = "-"))

election_frequency <- 
  ggplot(elite_df, aes(x = sarpanch_election_month_year_graph, fill = factor(direct))) +
  geom_bar(stat = "count", width = 16) +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1, size = 10)) +
  scale_x_date(labels = scales::date_format("%b-%Y"), date_breaks = "2 months") +  
  scale_y_log10() +
  labs(x = "Month-year", y = "Frequency (log scale)") +
  scale_fill_manual(values = c("gray", "black"), labels = c("Indirect", "Direct")) +
  labs(fill = "Election type")

ggsave("../results/figures/figure2.png", width=8, height=6, units = "in", election_frequency)

# Figure 3: Direct and indirect villages look similar on covariates

# Get covariates
covariates_for_balance <- c("number_villages_GP", 
                            "total_population_gp",
                            "overall_percent_SC_gp",
                            "km_from_block_office",
                            "total_time_from_block_office",
                            "speed_to_block_office",
                            "pilgrimage_sites",
                            "historic_sites",
                            "economic_opportunities",
                            "all_villages_electricity_grid",
                            "reserved_women",
                            "reserved_open",
                            "reserved_obc",
                            "reserved_sc")

# Run function 
balancetable <- run_ate_for_multiple_outcomes(elite_df, covariates_for_balance, "direct") %>% mutate(
  "Mean direct (SE)" = paste0(sprintf("%.3f", mean_treated), " (", sprintf("%.3f", se_treated), ")"),
  "Mean indirect (SE)" = paste0(sprintf("%.3f", mean_control), " (", sprintf("%.3f", se_control), ")"),
  "Diff (SE)" = paste0(sprintf("%.3f", coef_treatment.treatment), " (", sprintf("%.3f", se_treatment.treatment), ")")
) %>% rename(N = total_n) %>%
  select(Outcome,
             "Mean direct (SE)",
             "Mean indirect (SE)",
             "Diff (SE)",
         N,
         p_value.treatment)

# Format table
balancetable$Outcome <- c("No. villages", 
                        "Population", 
                        "Percent SC pop.",
                        "Distance to block (km)",
                        "Time to block (hr)",
                        "Speed to block (km/h)",
                        "Pilgrimage sites",
                        "Historic sites",
                        "Economic opportunities",
                        "Electricity grid",
                        "Gender quota",
                        "No caste quota",
                        "OBC caste quota",
                        "SC caste quota")


covariate_order <- c("No. villages", 
                     "Population", 
                     "Percent SC pop.",
                     "Distance to block (km)",
                     "Time to block (hr)",
                     "Speed to block (km/h)",
                     "Pilgrimage sites",
                     "Historic sites",
                     "Economic opportunities",
                     "Electricity grid",
                     "Gender quota",
                     "No caste quota",
                     "OBC caste quota",
                     "SC caste quota")

balancetable$Outcome <- factor(balancetable$Outcome, 
                               levels = rev(covariate_order), 
                               ordered = TRUE)


pvalueplot <- balancetable %>% 
  ggplot() + 
  geom_point(aes(x = p_value.treatment, y = Outcome), size = 1, colour = "black") +
  geom_text(aes(x = p_value.treatment, y = Outcome, label = round(p_value.treatment, 3)), 
            hjust = -0.1, size = 3) +  
  scale_x_continuous(name = "P-value", limits = c(0, 1.1), breaks = c(0, 0.5, 1)) +
  theme_minimal() + 
  theme(strip.background = element_rect(size = 0.5)) +
  theme(strip.placement = "outside") +
  theme(strip.text = element_blank()) +
  theme(panel.spacing = unit(0,'npc')) + 
  theme(strip.text.x = element_text(size = 10)) +
  theme(strip.text.y = element_text(size = 10)) +
  labs(x = "P-value",
       y = "Covariates",
       title = "Covariate by covariate t-tests") +
  geom_vline(data = data.frame(xint = 0.05), aes(xintercept = xint), 
             colour = "red", linetype = "longdash") + 
  geom_vline(data = data.frame(xint = 0.1), aes(xintercept = xint), 
             colour = "blue", linetype = "longdash") +
  annotate(geom = "text",
           label = c("p=0.05","p=0.1"),
           x = c(0.02, 0.07),
           y = c(10,10),
           angle = 90, 
           vjust = 1,
           size = 3) +
  theme(plot.title = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

table_data <- balancetable %>% select(-p_value.treatment)

table_data$Outcome <- factor(table_data$Outcome, 
                               levels = rev(covariate_order), 
                               ordered = TRUE)

tab_base <- ggplot(table_data, aes(y = Outcome)) +
  ylab(NULL) + 
  xlab("  ") + 
  theme(plot.title = element_text(hjust = 0.5, size = 12), 
        axis.text.x = element_text(color = "white"), 
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())

# Create individual tabs for each statistic using strings for display
tab1 <- tab_base + 
  geom_text(aes(x = 1, label = `Mean direct (SE)`), size = 3) + 
  labs(title = "Mean direct (SE)") +
  theme(plot.title = element_text(size = 10),
        axis.text.x = element_text(size = 10)) +
  scale_y_discrete(limits = levels(table_data$Outcome))

tab2 <- tab_base +
  geom_text(aes(x = 1, label = `Mean indirect (SE)`), size = 3)  + 
  labs(title = "Mean indirect (SE)") +
  theme(plot.title = element_text(size = 10),
        axis.text.x = element_text(size = 10))  +
  scale_y_discrete(limits = levels(table_data$Outcome))

tab3 <- tab_base +
  geom_text(aes(x = 1, label = `Diff (SE)`), size = 3)  + 
  labs(title = "Diff (SE)") +
  theme(plot.title = element_text(size = 10),
        axis.text.x = element_text(size = 10))  +
  scale_y_discrete(limits = levels(table_data$Outcome))

tab4 <- tab_base +
  geom_text(aes(x = 1, label = as.character(N)), size = 3)  + 
  labs(title = "N") +
  theme(plot.title = element_text(size = 10),
        axis.text.x = element_text(size = 10)) +
  scale_y_discrete(limits = levels(table_data$Outcome))  

# Ensure the y-axis limits are correctly set
tab1 <- tab1 + scale_y_discrete(limits = levels(table_data$Outcome))
tab2 <- tab2 + scale_y_discrete(limits = levels(table_data$Outcome))
tab3 <- tab3 + scale_y_discrete(limits = levels(table_data$Outcome))
tab4 <- tab4 + scale_y_discrete(limits = levels(table_data$Outcome))

# Define the layout for the combined plot 
lay <- matrix(1:5, nrow=1)

balance <- grid.arrange(pvalueplot, tab1, tab2, tab3, tab4, layout_matrix = lay, 
                        widths = c(4.3, 1.1, 1.1, 1.1, 0.5))

ggsave("../results/figures/figure3.png", width=9, height=6, units = "in", balance)

# Figure 4: Directly elected presidents enjoy more de facto authority

# Get DVs
vars <- c("most_influential_gd", "prop_speakingtime_sarpanch", "i_decide_masik_sabha")
clustered_vars <- c("sarpanch_nominate_BDO", "sarpanch_inauguralevents")

# Run the function
results <- run_regressions(vars, 
                           data_nonclustered = elite_df, 
                           data_clustered = citizen_df, 
                           clustered_vars = clustered_vars)

# Format results
results <- results %>%
  mutate(significance_level = case_when(
    p_value < 0.001 ~ 0.001,
    p_value > 0.001 & p_value < 0.01  ~ 0.01
  ))

results$title1 <- "Means, by type of election"

results$title2 <- "ATE estimate (diff. means)"

results$names <- c("President perceived as most influential actor in discussion",
                   "Proportion of time president spoke in discussion",
                   "President self-perception as main decision-maker in monthly meeting",
                   "Citizen nominated president as village representative to bureaucrat",
                   "Citizen reports president leads village events")

results <- results %>%
  mutate(
    indirect_mean_label = round(indirect_mean, 3),
    direct_mean_label = round(direct_mean, 3),
    indirectlabel = "i",
    directlabel = "d"
  )

results$names <- str_wrap(results$names, width = 25)

results$val <- 1:5

results$variable_type <- c("Perception", 
                           "Behavior",
                           "Perception",
                           "Behavior",
                           "Behavior")

means <- results %>%
  arrange(val) %>%
  mutate(names = factor(names, levels = names)) %>%
  ggplot() +
  geom_text(aes(x = names, y = direct_mean, label = directlabel), 
            vjust = 1, size = 5, color = "black") +
  geom_text(aes(x = names, y = indirect_mean, label = indirectlabel), 
            vjust = 1, size = 5, color = "black") +
  geom_text(aes(x = names, y = direct_mean, label = direct_mean_label), 
            vjust = -1.5, hjust = -0.1, size = 4, color = "black") +
  geom_text(aes(x = names, y = indirect_mean, label = indirect_mean_label), 
            vjust = -1.5, hjust = 0.75, size = 4, color = "black") +
  scale_y_continuous(name = "means", limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  facet_grid(variable_type ~ title1, scales = "free", switch = "y", space = "free_y") +
  theme_classic() +
  theme(
    strip.background = element_rect(size = 0.5),
    strip.placement = "outside",
    panel.spacing = unit(0, 'npc'),
    strip.text.x = element_text(size = 12),
    strip.text.y = element_text(size = 12),
    axis.text = element_text(size = 11),
    axis.line = element_line(color = 'black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    strip.text = element_text(size = 12)
  ) +
  coord_flip()

differences <- results %>% 
  arrange(val) %>%   
  mutate(names = factor(names, levels = names)) %>% 
  ggplot() + 
  geom_point(aes(x = names, y = ATE), size = 3, colour = "black") + 
  geom_linerange(aes(x = names, ymax = CI_upper_95, ymin = CI_lower_95), lwd = 1) +
  scale_y_continuous(name = "diff_means", limits = c(-0.05, 0.5), breaks = c(0, 0.5)) +
  facet_grid(variable_type ~ title2, scales = "free", switch = "y", space = "free_y") +
  theme_classic() +
  theme(
    strip.background = element_rect(size = 0.5),
    strip.placement = "outside",
    panel.spacing = unit(0, 'npc'),
    strip.text.x = element_text(size = 12),
    strip.text.y = element_blank(),  # Hides facet labels on the left
    axis.line = element_line(color = 'black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    strip.text = element_text(size = 12),
    axis.text = element_text(size = 11)
  ) +
  scale_x_discrete(labels = NULL, breaks = NULL) +
  labs(x = "") +
  coord_flip() +
  geom_hline(data = data.frame(yint = 0), aes(yintercept = yint), 
             colour = "grey50", linetype = "longdash") + 
  geom_text(aes(
    x = names, y = ATE, 
    label = ifelse(significance_level != "", 
                   sprintf("%.3f (p < %s)", ATE, significance_level), 
                   sprintf("%.3f", ATE))
  ), vjust = -0.9, hjust = 0.1, size = 4)

figure4 <- grid.arrange(means, 
                        differences, 
                        ncol=2, widths = c(1, 0.43))

ggsave("../results/figures/figure4.png", width=8, height=6, units = "in", figure4)


# Figure 5: Direct elections replace informal capture with formal capture

vars_formalcapture <- c("sarpanch_maratha", 
          "avg_prop_land_held_sarpanch_caste",
          "sarpanch_male",
          "sarpanch_land_own_name",
          "three_four_wheeler",
          "pacca_house")

# Run the function
results1 <- run_regressions(
  vars = vars_formalcapture,
  data_nonclustered = elite_df
)

results1$title1 <- "Means, by type of election"

results1$title2 <- "Direct election increases formal capture"

results1$names <- c("President Maratha",
                    "Proportion land held by president's caste in village",
                    "President male",
                    "President landed",
                    "President owns three/four-wheeler",
                    "President owns pacca house")

results1 <- results1 %>%
  mutate(
    indirect_mean_label = paste0(round(indirect_mean, 2), " (", round(indirect_mean_se, 2), ")"),
    direct_mean_label = paste0(round(direct_mean, 2), " (", round(direct_mean_se, 2), ")"),
    indirectlabel = "i",
    directlabel = "d"
  )

results1 <- results1 %>%
  mutate(significance_level = case_when(
    p_value < 0.001 ~ 0.001,
    p_value < 0.01 & p_value >= 0.001 ~ 0.01,
    p_value < 0.05 & p_value >= 0.01 ~ 0.05
  ),
  indirect_mean_label = round(indirect_mean, 3),
  direct_mean_label = round(direct_mean, 3),
  indirectlabel = "i",
  directlabel = "d"
  )

results1$names <- str_wrap(results1$names, width = 25)

results1$val <- 1:6

results1$variable_type <- c("Caste", 
                            "Caste",
                            "Gender",
                            "Class",
                            "Class",
                            "Class")



# informal capture

vars_informalcapture <- c("sarpanch_uncontested",
          "resignation",
          "rich_landowner_influences_gp")

clustered_vars_informalcapture <- c("other_nominate_BDO",
                    "formersarpanch_otherperson_inauguralevents",
                    "resignation_reported")

admin_vars_informalcapture <- c("resigned")

# Run the function
results2 <- run_regressions(
  vars = vars_informalcapture,
  data_nonclustered = elite_df,
  data_clustered = citizen_df,
  admin_data = admin_df,         
  clustered_vars = clustered_vars_informalcapture,
  admin_vars = admin_vars_informalcapture
)

results2 <- results2 %>%
  mutate(significance_level = case_when(
    p_value < 0.001 ~ 0.001,
    p_value < 0.01 & p_value >= 0.001 ~ 0.01,
    p_value < 0.05 & p_value >= 0.01 ~ 0.05
  ),
  indirect_mean_label = round(indirect_mean, 3),
  direct_mean_label = round(direct_mean, 3),
  indirectlabel = "i",
  directlabel = "d"
  )

results2$title1 <- "Means, by type of election"

results2$title2 <- "Direct election decreases informal capture"

results2$names <- c("President elected unopposed",
                    "President reports resignation",
                    "President reports rich landowner influences council",
                    "Citizen nominated former president/other villager as village representative to bureaucrat",
                    "Citizen reports former president/other villager leads village events",
                    "Citizen reports resignation",
                    "Resignation filed: Administrative data")

results2$names <- str_wrap(results2$names, width = 25)

results2$val <- 1:7

results2$variable_type <- c("Implicit",
                            "Implicit",
                            "Explicit", 
                            "Explicit",
                            "Explicit",
                            "Implicit",
                            "Implicit")

# formal capture panel

formal <- results1 %>% 
  arrange(val) %>%   
  mutate(names = factor(names, levels = names)) %>% 
  ggplot() + 
  geom_point(aes(x = names, y = ATE), size = 4, colour = "black") + 
  geom_linerange(aes(x = names, ymax = CI_upper_95, ymin = CI_lower_95), lwd = 1) +
  scale_y_continuous(name = "Difference in Means", limits = c(-0.5, 0.5), breaks = c(-0.5, 0, 0.5)) +
  facet_grid(variable_type ~ title2, scales = "free", switch = "y", space = "free_y") +
  theme_classic() +
  theme(
    strip.background = element_rect(size = 0.5),
    strip.placement = "outside",
    panel.spacing = unit(0, 'npc'),
    strip.text.x = element_text(size = 14),
    strip.text.y = element_text(size = 14),  # Shows facet labels on the left
    axis.line = element_line(color = 'black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title.y = element_text(size = 14),  # Restores y-axis title
    axis.text.y = element_text(size = 13),  # Restores y-axis text
    axis.text.x = element_text(size = 13)
  ) +
  labs(x = "") +
  ggtitle(" .") +
  coord_flip() +
  geom_hline(data = data.frame(yint = 0), aes(yintercept = yint), 
             colour = "grey50", linetype = "longdash") +
  geom_text(aes(
    x = names, y = ATE, 
    label = ifelse(significance_level != "", 
                   sprintf("Impact of direct: %.3f (p < %s)\nMean, indirect: %.3f", ATE, significance_level, indirect_mean), 
                   sprintf("Impact of direct: %.3f\nMean, indirect: %.3f", ATE, indirect_mean))
  ), hjust = 0.3, vjust = 0.5, size = 5) 

# informal capture panel

informal <- results2 %>% 
  arrange(val) %>%   
  mutate(names = factor(names, levels = names)) %>% 
  ggplot() + 
  geom_point(aes(x = names, y = ATE), size = 4, colour = "black") + 
  geom_linerange(aes(x = names, ymax = CI_upper_95, ymin = CI_lower_95), lwd = 1) +
  scale_y_continuous(name = "Difference in Means", limits = c(-0.5, 0.5), breaks = c(-0.5, 0, 0.5)) +
  facet_grid(variable_type ~ title2, scales = "free", switch = "y", space = "free_y") +
  theme_classic() +
  theme(
    strip.background = element_rect(size = 0.5),
    strip.placement = "outside",
    panel.spacing = unit(0, 'npc'),
    strip.text.x = element_text(size = 14),
    strip.text.y = element_text(size = 14),  # Shows facet labels on the left
    axis.line = element_line(color = 'black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title.y = element_text(size = 14),  # Restores y-axis title
    axis.text.y = element_text(size = 13),  # Restores y-axis text
    axis.text.x = element_text(size = 13)
  ) +
  labs(x = "") +
  ggtitle(" .") +
  coord_flip() +
  geom_hline(data = data.frame(yint = 0), aes(yintercept = yint), 
             colour = "grey50", linetype = "longdash") +
  geom_text(aes(
    x = names, y = ATE, 
    label = ifelse(significance_level != "", 
                   sprintf("Impact of direct: %.3f (p < %s)\nMean, indirect: %.3f", ATE, significance_level, indirect_mean), 
                   sprintf("Impact of direct: %.3f\nMean, indirect: %.3f", ATE, indirect_mean))
  ), hjust = 0.8, vjust = 0.5, size = 5) 

# save two panels

formalinformal <- grid.arrange(formal, informal, nrow = 2)


ggsave("../results/figures/figure5.png", formalinformal, width = 12, height = 16, units = "in")


# Figure 6: Demographic dominance boosts de facto authority

# Define outcome variable
outcome <- "authority"

# Define list of independent variables
independent_vars <- c("Male", "Maratha")

# Run the function and store the results in a dataframe
results_df <- run_regressions_surveyexperiment(experiment_df, outcome, independent_vars)

results_df <- results_df %>%
  mutate(significance_level = case_when(
    P_Value < 0.001 ~ 0.001,
    P_Value < 0.01  ~ 0.01,
    P_Value < 0.05  ~ 0.05,
    P_Value < 0.1   ~ 0.1
  ))


# make graph
plot1 <- results_df %>%
  mutate(Independent_Var = factor(Independent_Var, levels = Independent_Var),
         Label = case_when(
           Independent_Var == "Maratha" ~ sprintf("Mean Dalit: %.3f", Mean_Indep_0),
           Independent_Var == "Male" ~ sprintf("Mean female: %.3f", Mean_Indep_0),
           TRUE ~ sprintf("Control Mean: %.3f", Mean_Indep_0)  
         )) %>%
  ggplot() + 
  geom_point(aes(x = Independent_Var, y = ATE), size = 3, colour = "black") + 
  geom_linerange(aes(x = Independent_Var, ymax = CI_Upper, ymin = CI_Lower), lwd = 1) +
  scale_y_continuous(name = "Covariate-adjusted OLS ATE estimate (percentage points)", limits = c(-0.05, 0.25), breaks = c(0, 0.25)) +
  theme_classic() +
  theme(
    strip.background = element_rect(size = 0.5),
    strip.placement = "outside",
    panel.spacing = unit(0, 'npc'),
    strip.text.x = element_text(size = 12),
    axis.line = element_line(color = 'black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.text = element_text(size = 11)
  ) +
  labs(x = "", y = "ATE") + 
  ggtitle("Demographic dominance increases authority") + 
  coord_flip() +
  geom_hline(aes(yintercept = 0), colour = "grey50", linetype = "longdash") + 
  geom_text(aes(
    x = Independent_Var, 
    y = ATE, 
    label = sprintf("ATE estimate: %.3f\n%s\n(p < %.3f)", ATE, Label, significance_level)
  ), vjust = -0.8, hjust = 0.3, size = 4)


df_long <- experiment_df %>%
  select(sarpanchgendercaste_combination, authority) %>%
  pivot_longer(cols = c(authority),
               names_to = "variable", values_to = "value")

df_long$variable <- factor(df_long$variable, levels = c("authority"))

mean_values <- df_long %>%
  group_by(sarpanchgendercaste_combination) %>%
  summarise(mean_value = mean(value, na.rm = TRUE))

mean_values$sarpanchgendercaste_combination <- factor(
  mean_values$sarpanchgendercaste_combination, 
  levels = c("rohit marathe", "bharti marathe", "rohit kamble", "bharti kamble")
)

plot2 <- ggplot(mean_values, aes(x = sarpanchgendercaste_combination, y = mean_value)) +
  geom_bar(stat = "identity", position = "dodge", fill = "darkgray") +
  geom_text(aes(label = round(mean_value, 2)), 
            vjust = -0.5, size = 4) +  # Add mean values as text labels
  theme_minimal() +
  labs(x = "Hypothetical president", y = "Mean value", title = "Intersectional dominance increases authority") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("authority" = "darkgray"))

combined_plot <- grid.arrange(plot1, plot2, ncol = 2)

# Save the combined plot as a PNG
ggsave("../results/figures/figure6.png", combined_plot, width = 12, height = 6, units = "in", dpi = 300)
