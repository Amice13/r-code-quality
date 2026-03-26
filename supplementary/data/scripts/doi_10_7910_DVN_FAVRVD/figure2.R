############ Setup ##########
# This analysis was conducted using R Studio Version 2025.05.0+496 for Mac and R version 4.5.0 for Mac
remove(list=ls())
set.seed(8675309)

# remember to set working directory to source file location

pacman::p_load(ggplot2,readxl,scales) # load relevant packages

## Load literature review data ##
data <- read_excel("literature_review.xlsx", sheet = "Sheet1") # load original literature review
# contains all identified publications using individual-level bureaucrat staff data
# both in IOs and in state foreign policy bureaucracies
# associated with the staff size of each IO
# includes short form references to specific articles -- see works cited for complete citation

# Include all IOs (even if no articles) and all foreign policy bureaucracies for which we identified articles
data <- data[(data$type == "IO") | (data$n_articles > 0), ]

# Reorder IOs so that they appear by ascending staff size
data <- data[order(data$staffsize,data$institution_graphic), ]

# Reshape data for side-by-side bars in the IO plot, representing no. of articles and staff size
data_long <- tidyr::pivot_longer(data, cols = c(staffsize, n_articles), names_to = "Metric", values_to = "Value")

# Adjust scaling factor to balance the visualization of no. of articles vs. staff size
scaling_factor <- max(data$staffsize, na.rm=TRUE) / max(data$n_articles)
data_long$Value <- ifelse(data_long$Metric == "n_articles", data_long$Value * scaling_factor, data_long$Value)

# Relabel variable names
data_long$Metric <- ifelse(data_long$Metric == "n_articles", "Number of articles", "Staff size")

# Ensure institutions maintain order in the plot
data_long$institution_graphic <- factor(data_long$institution_graphic, levels = unique(data$institution_graphic))

# Create the plot
ggplot(data_long, aes(x = institution_graphic, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_continuous(
    name = "Staff Size",
    sec.axis = sec_axis(~ . / scaling_factor, name = "Number of Articles", breaks = pretty_breaks())
  ) +
  scale_fill_grey() +
  facet_wrap( ~ type_graphic, strip.position = "bottom", scales = "free_x") + 
  labs(x = "Institution", fill = "Metric") +
  theme_minimal() + theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
# no observations for staff size of 7 included foreign policy bureaucracies, so these drop out
ggsave("literature_review.pdf")
