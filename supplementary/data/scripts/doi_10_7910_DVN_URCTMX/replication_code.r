# load libraries

library(ggplot2)
library(dplyr)
library(gridExtra) # for arranging two plots 

# load data 

setwd("Z:\\projects\\covid19\\share\\working_papers\\okinawa\\manuscript")

jp <- read.csv("replication_data.csv", header=TRUE)

# descriptives for table 2

length(jp$id)               # number of respondents
range(jp$age)               # age range
round(mean(jp$age), 2)      # mean age
median(jp$age)              # media age
round(sd(jp$age), 2)        # standard deviation age

# function to create plot for figure 1

plot_prefecture_ranking <- function(df, variable, title, xCaption, xWidth) {
  ranked_df <- df %>%
    group_by(pref, pref_eng) %>%
    summarise(mean_value = mean(.data[[variable]], na.rm = TRUE),
              std_dev = sd(.data[[variable]], na.rm = TRUE)) %>%
    arrange(mean_value)  
  
  # create a colour column to highlight Okinawa
  
  ranked_df <- ranked_df %>%
    mutate(color = ifelse(pref_eng == "Okinawa", "red", "skyblue"))
  
  # now plot
  ggplot(ranked_df, aes(x = mean_value, y = reorder(pref_eng, mean_value))) +
    geom_bar(stat = "identity", aes(fill = color), color = "black") +
    geom_errorbarh(aes(xmin = mean_value - std_dev, xmax = mean_value + std_dev), height = 0.3) +
    geom_text(aes(label = paste0(round(mean_value, 2), " (", round(std_dev, 2), ")"),
                  x = mean_value + std_dev + 0.1), # Position text to the right of error bar
              hjust = 0, size = 3.5) +
    scale_fill_identity() +  # Use the colors as defined in the data
    scale_x_continuous(limits = c(0, xWidth)) + 
    labs(title = title,
         x = xCaption,
         y = "Prefecture") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 10))
}

# now call the function to draw the plots

# first draw to screen 

plot_prefecture_ranking(jp, "vaccinations", "Number of COVID-19 vaccinations", "Mean (standard deviation)", 5.7)
plot_prefecture_ranking(jp, "vaccineIsEffective", "The COVID-19 if effective in preventing people from catching COVID-19", "Mean level of agreement (standard deviation)", 7)

# now draw both to one page in a pdf

p1 <- plot_prefecture_ranking(jp, "vaccinations", "Number of COVID-19 vaccinations", "Mean (standard deviation)", 5.7)
p2 <- plot_prefecture_ranking(jp, "vaccineIsEffective", "The COVID-19 vaccine is effective", "Mean level of agreement (standard deviation)", 7)

pdf("plots.pdf", width = 29.7 / 2.54, height = 42 / 2.54)
grid.arrange(p1, p2, ncol = 2)

dev.off()

# models 

# first, a function to extract the estimates and format the p-values

extract_results <- function(model, model_name) {
  broom::tidy(model, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%  # remove intercept
    select(term, estimate, conf.low, conf.high, p.value) %>%
    rename(Variable = term, Estimate = estimate, `CI Lower` = conf.low, `CI Upper` = conf.high, `p-value` = p.value) %>%
    mutate(
      `p-value` = ifelse(`p-value` < 0.001, "<0.001", format(round(`p-value`, 3), nsmall = 3)),  # Ensure p-value is always character
      Model = model_name
    ) %>%
    mutate(across(where(is.numeric), round, 3))  # round numeric values for better readability
}

# now to build the models

# 1. vaccinations

# unadjusted models (one predictor at a time)

unadjusted_models <- lapply(
  c("okinawa", "women", "trustHealthcare", "trustGovernment", "age", "ageSquared", "facemasks", "healthStatus", "covidThreatPersonally", "covidThreatCountry", "compulsory", "ldp", "education", "islands_pop_rate", "doctorsPer100k", "rate_Moderna", "cum_patients_pc"),
  function(var) lm(as.formula(paste("vaccinations ~", var)), data = jp)
)

# extract results from unadjusted models

unadjusted_results <- lapply(unadjusted_models, extract_results, "Unadjusted") %>% bind_rows()

# adjusted model

adjusted_model <- lm(vaccinations ~ okinawa + women + trustHealthcare + trustGovernment + age + ageSquared + facemasks + healthStatus + covidThreatPersonally + covidThreatCountry + compulsory + ldp + education + islands_pop_rate + doctorsPer100k + rate_Moderna + cum_patients_pc, data = jp)

# extract results from adjusted model

adjusted_results <- extract_results(adjusted_model, "Adjusted")

# display results

unadjusted_results
adjusted_results



# 2. vaccineIsEffective

# unadjusted models (one predictor at a time)

unadjusted_models <- lapply(
  c("okinawa", "women", "trustHealthcare", "trustGovernment", "age", "ageSquared", "facemasks", "healthStatus", "covidThreatPersonally", "covidThreatCountry", "compulsory", "ldp", "education", "islands_pop_rate", "doctorsPer100k", "rate_Moderna", "cum_patients_pc"),
  function(var) lm(as.formula(paste("vaccineIsEffective ~", var)), data = jp)
)

# extract results from unadjusted models

unadjusted_results <- lapply(unadjusted_models, extract_results, "Unadjusted") %>% bind_rows()


# adjusted model

adjusted_model <- lm(vaccineIsEffective ~ okinawa + women + trustHealthcare + trustGovernment + age + ageSquared + facemasks + healthStatus + covidThreatPersonally + covidThreatCountry + compulsory + ldp + education + islands_pop_rate + doctorsPer100k + rate_Moderna + cum_patients_pc, data = jp)

# extract results from adjusted model

adjusted_results <- extract_results(adjusted_model, "Adjusted")

# display results

unadjusted_results
adjusted_results

