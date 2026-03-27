########################################################
# Wolf in Sheep's Clothing - Main Text Replication File
# Chiopris, Nalepa and Vanberg
# Main Text
########################################################

# Set up workspace
rm(list=ls())
library(tidyverse)
library(readxl)
library(survey)
library(stargazer)
library(glue)
library(kableExtra)
library(broom)
library(knitr)
library(stringr)

# Working directory - change as necessary
setwd("~/Dropbox/Auth Backsliding/REPLICATION MARCH 2024/")

# Load data
data <- read_csv("data.csv")

# Remove unnecessary first column with row numbers
data <- data %>% dplyr::select(-"...1")

# This will properly add weights to each observation
design_cbos <- svydesign(ids=~1, probs=NULL, 
                         strata=NULL, data = data, weights=data$waga)

# Create string with all province names to add dummies in regression formula
p <- data %>% dplyr::select(starts_with("province_")) 
p <- stringr::str_c(colnames(p), collapse = " + ")



#---------------------------------------------------------
# Table 4
#---------------------------------------------------------

## For each group, how many vote for candidate B in treatment and control


# Find means
perc_voteB <- data %>% group_by(treatment, v_undem_fourgroups) %>%
  summarize(mean = mean(candidate, na.rm=T))

# Remove NAs
perc_voteB <- perc_voteB %>% filter(!is.na(v_undem_fourgroups))

# Pivot wider and rename columns
perc_voteB <- perc_voteB %>% pivot_wider(names_from = treatment, values_from = mean)
colnames(perc_voteB) <- c("Group", "Control", "Treatment")

# Find percentage change
perc_voteB$difference <- (round(perc_voteB$Treatment, digits=2) - round(perc_voteB$Control, digits=2))
perc_voteB$percentage_change <- (perc_voteB$Treatment -perc_voteB$Control)/perc_voteB$Control

# Include group names for clarity
perc_voteB <- perc_voteB %>%
  mutate(Group = case_when(
    Group == 1 ~ "Low agreement",
    Group == 2 ~ "Mid-low agreement",
    Group == 3 ~ "Mid-high agreement",
    Group == 4 ~ "High agreement"
  ))

# Kable to print table in a tidy way
table_output <- capture.output(kable(perc_voteB, digits = 2))

# Write the output to a text file
writeLines(table_output, "output/table4.txt")
rm(table_output)


#---------------------------------------------------------
# Table 5
#---------------------------------------------------------

# Column 1: province FE, no interaction

# Write formula
ff_withFE_10_nointer <- as.formula(paste("pis", 
                                         paste("v_undem", "l_r", "democracy_index",
                                               "age", "edu", "town", "unemployed","dissatisfaction", "female",
                                               "religiosity", "l_r","interest_pol", 
                                               p, sep=" + "), sep = " ~ " ))
# Run regression
m1 <- svyglm(ff_withFE_10_nointer, design = design_cbos, data=data, family=quasibinomial)

# Column 2: no province FE, interaction

# Write formula
ff_withoutFE_10 <- as.formula(paste("pis", 
                                    paste("v_undem*l_r","democracy_index",
                                          "age", "edu", "town", "unemployed",  "dissatisfaction", "female", 
                                          "religiosity", "l_r","interest_pol", 
                                          sep=" + "), sep = " ~ " ))
# Run regression
m2 <- svyglm(ff_withoutFE_10, design = design_cbos, data=data, family=quasibinomial)

# Column 3: province FE, interaction

# Write formula
ff_withFE_10 <- as.formula(paste("pis", 
                                 paste("v_undem*l_r", "democracy_index",
                                       "age", "edu", "town", "unemployed","dissatisfaction", "female",
                                       "religiosity", "l_r","interest_pol", 
                                       p, sep=" + "), sep = " ~ " ))
# Run regression
m3 <- svyglm(ff_withFE_10, design = design_cbos, data=data, family=quasibinomial)



# Turn into tables that are easy to read
tidy_m1 <- tidy(m1) %>%
  mutate(column = "(1)")  %>%
  select(term, column, estimate, std.error, p.value) %>%
  mutate(across(c(estimate, std.error, p.value), 
                ~round(., digits = 3)))
  
tidy_m2 <- tidy(m2) %>%
  mutate(column = "(2)")%>%
  select(term, column,estimate, std.error, p.value) %>%
  mutate(across(c(estimate, std.error, p.value), 
                ~round(., digits = 3)))

tidy_m3 <- tidy(m3) %>%
  mutate(column = "(3)") %>%
  select(term, column,estimate, std.error, p.value) %>%
  mutate(across(c(estimate, std.error, p.value), 
                ~round(., digits = 3)))
  
# Combine the results into one data frame
combined_results <- rbind(tidy_m1, tidy_m2, tidy_m3)

# Turn p-values into stars
 # Write function
p_value_stars <- function(p) {
  ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", "")))
}
 # Use function
combined_results <- combined_results %>%
    mutate(significance = sapply(p.value, p_value_stars)) %>%
    select(-p.value)

# Turn into wide format
combined_results_wide <- combined_results %>%
  pivot_wider(names_from = column, 
              values_from = c(estimate, std.error, significance))

# Rename the variables for legibility
combined_results_wide <- combined_results_wide %>%
  mutate(Variable = case_when(
    term == "v_undem" ~ "Undemocratic interpretation",
    term ==  "l_r" ~ "Self-placement in left-right spectrum",
    term ==  "democracy_index" ~ "Expressed Preferences for Democracy",
    term ==  "edu" ~ "Education",
    term == "town" ~ "Town Size",
    term ==  "unemployed" ~ "Unemployed",
    term ==  "dissatisfaction" ~ "Dissatisfaction with one’s condition",
    term ==  "female" ~ "Gender",
    term ==  "religiosity" ~ "Religiosity",
    term ==  "interest_pol" ~ "Interest in politics",
    term ==  "age" ~ "Age",
    term ==  "v_undem:l_r" ~ "Undemocratic interpretation x left-right",
  ))

# Select only relevant columns and turn province FE into one row for readability
results <- combined_results_wide %>% 
  dplyr::select(c("Variable", "estimate_(1)", "std.error_(1)", 
                  "significance_(1)",
                "estimate_(2)", "std.error_(2)", "significance_(2)",
                "estimate_(3)", "std.error_(3)", "significance_(3)")) %>%
  filter(!is.na(Variable)) %>% 
  rbind(c("Province FE", "Yes", "", "", "No", "", "", "Yes", "", "")) %>%
  rbind(c("Number of Observations", nobs(m1), "", "", 
          nobs(m2),"", "", nobs(m3), "", "")) %>% # Add number of observations 
  rename( # rename variables
    est1 = "estimate_(1)", st.err1 = "std.error_(1)", sig1 = "significance_(1)",
    est2 = "estimate_(2)",  st.err2 = "std.error_(2)", sig2 = "significance_(2)",
    est3 = "estimate_(3)", st.err3 = "std.error_(3)", sig3 = "significance_(3)"
  ) %>% # change "NA" into empty space
  mutate(across(everything(), ~if_else(is.na(.), "", as.character(.))))
# Rearrange rows so that more important are on top
results <- results[c(1,2,12,3:11,13,14),]

# Remove unnecessary dataframes
rm(combined_results, combined_results_wide, tidy_m1, tidy_m2, tidy_m3)

# Create a table with kable
table_output <- kable(results)

# Capture the table output
captured_output <- capture.output(table_output)

# Write the output to a text file
writeLines(captured_output, "output/table5.txt")


#---------------------------------------------------------
# Figure 6
#---------------------------------------------------------


# Model
mod_inter_cj <- lm(data= data, candidate ~ treatment*as.factor(v_undem_fourgroups))
coefeq <- matrix(data=0, nrow=1, ncol=length(mod_inter_cj$coefficients))
colnames(coefeq) <- names(mod_inter_cj$coefficients)
coefeq[1, "treatment"] <- 1

# ACME for group 1
coefeq1 <- coefeq
wht1 <- multcomp::glht(mod_inter_cj, linfct = coefeq1)
g1ci <- confint(wht1) %>% tidy

# ACME for group 2
coefeq2 <- coefeq
coefeq2[1,"treatment:as.factor(v_undem_fourgroups)2"] <- 1
wht2 <- multcomp::glht(mod_inter_cj, linfct = coefeq2)
g2ci <- confint(wht2) %>% tidy

# ACME for group 3
coefeq3 <- coefeq
coefeq3[1,"treatment:as.factor(v_undem_fourgroups)3"] <- 1
wht3 <- multcomp::glht(mod_inter_cj, linfct = coefeq3)
g3ci <- confint(wht3) %>% tidy

# ACME for group 4
coefeq4 <- coefeq
coefeq4[1,"treatment:as.factor(v_undem_fourgroups)4"] <- 1
wht4 <- multcomp::glht(mod_inter_cj, linfct = coefeq4)
g4ci <- confint(wht4) %>% tidy

# Dataframe for plot
data_conditional_interaction <- data.frame(
  group = c("Low", "Mid-Low", "Mid-High", "High"),
  estimate = c(g1ci$estimate, g2ci$estimate, g3ci$estimate, g4ci$estimate),
  lower = c(g1ci$conf.low,g2ci$conf.low,g3ci$conf.low,g4ci$conf.low), 
  upper = c(g1ci$conf.high,g2ci$conf.high,g3ci$conf.high,g4ci$conf.high))

data_conditional_interaction$group <- factor(data_conditional_interaction$group, 
                                             levels =data_conditional_interaction$group)

# Create Plot
plot_conditional_inter <- ggplot(data= data_conditional_interaction, 
                                 aes(x= group, y= estimate)) + 
  geom_point(size=1.7) + theme_bw() + geom_linerange(aes(ymin=lower, ymax=upper, 
                                                         xmin=group), size=0.3) +
  geom_hline(aes(yintercept=0), linetype = "solid", color="brown4", size=0.5) +
  labs(x ="Interpretation of vignette reform as undemocratic", 
       y ="Punishment for undemocratic proposal") + 
  theme(axis.ticks.x =element_blank(),
        axis.title=element_text(size=10),
        panel.grid.minor = element_blank(),
        legend.position = "none") 

# Save plot
ggsave(filename="output/figure6.pdf", 
       plot = plot_conditional_inter,
       width = 5, height = 4, dpi = 300, units = "in", device='pdf')


#---------------------------------------------------------
# Figure 7
#---------------------------------------------------------


# Identify right-wing voters
right_voters <- data[data$l_r>=5 & !is.na(data$l_r),]

# Find median of variables
data_predictions <- data.frame(
  l_r = rep(seq(5, 7, by=1), each=10),
  v_undem = rep(seq(1,10, by=1), 3), 
  v_acc = median(right_voters$v_acc, na.rm = T),     
  age = median(right_voters$age, na.rm = T), 
  edu= median(right_voters$edu, na.rm = T), 
  town = median(right_voters$town, na.rm = T), 
  unemployed=  median(right_voters$unemployed, na.rm = T), 
  dissatisfaction = median(right_voters$dissatisfaction, na.rm = T), 
  female = median(right_voters$female, na.rm = T), 
  religiosity = median(right_voters$religiosity, na.rm = T), 
  interest_pol = median(right_voters$interest_pol, na.rm = T), 
  democracy_index = median(right_voters$democracy_index, na.rm = T))

# Create dataframe with predictions
predictions <- data.frame(l_r = data_predictions$l_r,
               v_undem = data_predictions$v_undem, 
               predict.glm(m2, data_predictions,type="response", se.fit = T))
# Only keep relevant columns
predictions <- predictions %>% select(-residual.scale)
# Create confidence intervales
predictions$lower <- predictions$fit - 1.96*predictions$se.fit
predictions$upper <- predictions$fit + 1.96*predictions$se.fit
# Save as factor
predictions$l_r <- as.factor(predictions$l_r)



# Plot for each of the three groups

# Self-placement at 5
lr_group=5
# Select relevant data to use for plot
current_data <- predictions[predictions$l_r == lr_group, ]
hist_data <- right_voters[right_voters$l_r == lr_group, ]
hist_data <- hist_data %>% group_by(v_undem) %>% summarise(count = n())
current_data <- merge(current_data, hist_data, by="v_undem")
# Scale to align y axis on the left and right
scale =60

# Plot for 5
plot <- ggplot(current_data, aes(x=v_undem)) + 
  geom_point( aes(x = v_undem, y = fit), inherit.aes = FALSE) +  
  geom_bar(aes(y= count/scale), 
           fill= "lightgray", alpha=0.6,stat = "identity") + 
  geom_linerange(data = current_data, 
                 aes(ymin = lower, ymax = upper, x = v_undem), 
                 color = "gray10") +
  geom_ribbon(data = current_data,
              aes(ymin = lower, ymax = upper, x = v_undem),
              fill = "darkgray", 
              alpha = 0.9, color = NA, inherit.aes = FALSE) +
  xlab("") + 
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10), labels = c(2, 4, 6, 8, 10)) +
  scale_y_continuous(
    name = "Predicted Probabilities of PiS Voting",
    limits = c(0, 1),
    sec.axis = sec_axis(~ . * 60, name = "", 
                        breaks = c(0, 20, 40, 60))
  ) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        axis.text = element_text(size = 30),  # Increase the size of axis ticks
        axis.title = element_text(size = 30) ) 
# Save plot
ggsave(glue("output/figure7_", "a", ".pdf"),
       plot = plot,
       width = 9, height = 9, dpi = 300, units = "in", device='pdf')


# Self-placement at 6

# Same as above
lr_group=6
current_data <- predictions[predictions$l_r == lr_group, ]
hist_data <- right_voters[right_voters$l_r == lr_group, ]
hist_data <- hist_data %>% group_by(v_undem) %>% summarise(count = n())
current_data <- merge(current_data, hist_data, by="v_undem")

# Plot 
plot <- ggplot(current_data, aes(x=v_undem)) + 
  geom_point( aes(x = v_undem, y = fit), inherit.aes = FALSE) +  
  geom_bar(aes(y= count/scale), 
           fill= "lightgray", alpha=0.6,stat = "identity") + 
  geom_linerange(data = current_data, 
                 aes(ymin = lower, ymax = upper, x = v_undem), 
                 color = "gray10") +
  geom_ribbon(data = current_data,
              aes(ymin = lower, ymax = upper, x = v_undem),
              fill = "darkgray", 
              alpha = 0.9, color = NA, inherit.aes = FALSE) +
  xlab("Reform Interpretation") + 
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10), labels = c(2, 4, 6, 8, 10)) +
  scale_y_continuous(
    name = "",
    limits = c(0, 1),
    sec.axis = sec_axis(~ . * 60, name = "", 
                        breaks = c(0, 20, 40, 60))
  ) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        axis.text = element_text(size = 30),  # Increase the size of axis ticks
        axis.title = element_text(size = 30) ) 

# Save plot
ggsave(glue("output/figure7_", "b", ".pdf"),
       plot = plot,
       width = 9, height = 9, dpi = 300, units = "in", device='pdf')

# Self-placement at 7

# Same as above
lr_group=7
current_data <- predictions[predictions$l_r == lr_group, ]
hist_data <- right_voters[right_voters$l_r == lr_group, ]
hist_data <- hist_data %>% group_by(v_undem) %>% summarise(count = n())
current_data <- merge(current_data, hist_data, by="v_undem")

# PLot
plot <- ggplot(current_data, aes(x=v_undem)) + 
  geom_point( aes(x = v_undem, y = fit), inherit.aes = FALSE) +  
  geom_bar(aes(y= count/scale), 
           fill= "lightgray", alpha=0.6,stat = "identity") + 
  geom_linerange(data = current_data, 
                 aes(ymin = lower, ymax = upper, x = v_undem), 
                 color = "gray10") +
  geom_ribbon(data = current_data,
              aes(ymin = lower, ymax = upper, x = v_undem),
              fill = "darkgray", 
              alpha = 0.9, color = NA, inherit.aes = FALSE) +
  xlab("") + 
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10), labels = c(2, 4, 6, 8, 10)) +
  scale_y_continuous(
    name = "",
    limits = c(0, 1),
    sec.axis = sec_axis(~ . * 60, name = "Count", 
                        breaks = c(0, 20, 40, 60))
  ) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        axis.text = element_text(size = 30),  # Increase the size of axis ticks
        axis.title = element_text(size = 30) ) 

# Save plot
ggsave(glue("output/figure7_", "c", ".pdf"),
       plot = plot,
       width = 9, height = 9, dpi = 300, units = "in", device='pdf')




