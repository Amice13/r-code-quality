################################################################################
  
# Replication code for:
#   
#   "Beyond Borders: Does Firm-Level Exposure to State and Local Paid Sick Leave 
# 	Mandates Lead to Policy Diffusion?"
# 
#   Daniel Schneider, Harvard University
# 
#   Kristen Harknett, UC San Francisco
# 
#   Journal of Policy Analysis & Management
# 
#   September 2025
# 
# 
# CREATES: Figure 3 + Appendix Figure 1 (all others created in run_all_Stata_exhibits.do)
#
# MUST UPDATE LINE 37 BEFORE RUNNING

################################################################################

# necessary packages #
library(haven)
library(dplyr)
library(broom)
library(ggplot2)
library(margins)
library(fixest)
library(marginaleffects)
library(mice)
library(tidyr)
library(patchwork)
library(grid)


system_prefix <- "UPDATE" # UPDATE AS NECESSARY!




####################
##### FIGURE 3 #####
####################

# import data
data <- read_dta(file.path(system_prefix, "data/main_analysis_file.dta"))
data <- data %>% 
  filter(!(`_mi_m` < 0 | `_mi_m` > 10)) %>%
  filter(psl_law_places_hybd == 0)

# define control vectors
demog <- c("gender", "race2_harm", "age_text_clean", "kids", "eslhome", "enrolled", "cohabstatus")
work <- c("manager", "longwork_yrs", "union", "hourwage_clean", "usualhours_clean")

formula_vars <- c("pct_emps_psl_place", demog, work, "statelist", "year*month")
formula_str <- paste("benefits_paidsick ~", paste(formula_vars, collapse=" + "))

# estimations for each imputation
margin_values <- seq(0, 50, 5)
all_predictions <- list()


for(m in 1:10) {
  print(paste("Processing imputation", m))
  
  # Filter to imputation m
  data_m <- data %>% filter(`_mi_m` == m)
  
  if(nrow(data_m) == 0) {
    print(paste("No data for imputation", m))
    next
  }
  
  model_m <- feols(as.formula(formula_str), 
                   data = data_m, 
                   cluster = ~statelist)

  pred_m <- avg_predictions(model_m, 
                            newdata = datagrid(pct_emps_psl_place = margin_values,
                                               grid_type = "counterfactual"),
                            by = "pct_emps_psl_place")
  
  all_predictions[[m]] <- as.data.frame(pred_m)
}


# combining results w/ rubin's rules
pooled_results <- data.frame(
  pct_emps_psl_place = margin_values,
  estimate = numeric(length(margin_values)),
  std.error = numeric(length(margin_values)),
  conf.low = numeric(length(margin_values)),
  conf.high = numeric(length(margin_values))
)

for(j in 1:length(margin_values)) {
  estimates <- sapply(all_predictions, function(x) x$estimate[j])
  std_errors <- sapply(all_predictions, function(x) x$std.error[j])
  
  Q_bar <- mean(estimates, na.rm = TRUE)  # pooled estimate
  U_bar <- mean(std_errors^2, na.rm = TRUE)  # within-imputation variance
  B <- var(estimates, na.rm = TRUE)  # between-imputation variance
  m_obs <- sum(!is.na(estimates))  # number of complete imputations
  T <- U_bar + (1 + 1/m_obs) * B  # total variance
  
  pooled_results$estimate[j] <- Q_bar
  pooled_results$std.error[j] <- sqrt(T)
  pooled_results$conf.low[j] <- Q_bar - 1.96 * sqrt(T)
  pooled_results$conf.high[j] <- Q_bar + 1.96 * sqrt(T)
}

margins_df <- pooled_results

# plotting
plot <- ggplot(margins_df, aes(x = pct_emps_psl_place, y = estimate)) +
  geom_point(size = 2.5, shape = 1, color = "black", stroke = 1) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.5) +
  scale_x_continuous(
    breaks = seq(0, 50, 5),
    labels = paste0(seq(0, 50, 5), "%")
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.5, 0.05),
    labels = paste0(seq(0, 50, 5), "%")
  ) +
  labs(
    title = "",
    x = "% of Firm's Workers in PSL Mandate Places",
    y = "% of Workers in Non-PSL Mandate Places\nReporting Access to PSL"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 13),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    plot.title = element_text(size = 14)
  )

print(plot)


# saving
ggsave(file.path(system_prefix, "results/exhibits/Figure 3.pdf"), plot, width = 8, height = 6)









#############################
##### APPENDIX FIGURE 1 #####
#############################

# import data
data <- read_dta(file.path(system_prefix, "data/main_analysis_file.dta"))
data <- data %>% 
  filter(!(`_mi_m` < 0 | `_mi_m` > 10)) %>%
  filter(psl_law_places_hybd == 0)

# define control vectors
demog <- c("gender", "race2_harm", "age_text_clean", "kids", "eslhome", "enrolled", "cohabstatus")
work <- c("manager", "longwork_yrs", "union", "hourwage_clean", "usualhours_clean")
statechars <- c("unionmem_pc", "unemp_rate", "minwage_state", "race_black", "race_amind", 
                "race_asian", "race_pacisl", "race_other", "race_multi", "hispan",
                "age_u5", "age_5to9", "age_10to14", "age_75to84", "age_85plus", 
                "female_pc", "educ_pc_hsgrad", "educ_pc_somecoll", "educ_pc_assoc", 
                "educ_pc_bach", "educ_pc_grad")

outcomes <- c("benefits_paidsick", "benefits_health", "benefits_dental", 
              "benefits_paidleave", "benefits_retirementplan", "benefits_tuition")

titles <- c("Paid Sick Leave", "Health Insurance", "Dental Insurance",
            "Paid Leave", "Retirement Plan", "Tuition Assistance")


# running for each outcome
run_mi_analysis_working <- function(outcome) {
  print(paste("Processing outcome:", outcome))
  
  # Use your working formula structure
  formula_vars <- c("pct_emps_psl_place", demog, work, statechars, "statelist", "year*month")
  formula_str <- paste(outcome, "~", paste(formula_vars, collapse=" + "))
  
  margin_values <- seq(0, 50, 5)
  all_predictions <- list()
  
  for(m in 1:10) {
    print(paste("  Processing imputation", m, "for", outcome))
    
    data_m <- data %>% filter(`_mi_m` == m)
    
    if(nrow(data_m) == 0) {
      print(paste("  No data for imputation", m))
      next
    }
    
    model_m <- feols(as.formula(formula_str), 
                     data = data_m, 
                     cluster = ~statelist)

    pred_m <- avg_predictions(model_m, 
                              newdata = datagrid(pct_emps_psl_place = margin_values,
                                                 grid_type = "counterfactual"),
                              by = "pct_emps_psl_place")
    
    all_predictions[[m]] <- as.data.frame(pred_m)
  }

  
# combining results
pooled_results <- data.frame(
    pct_emps_psl_place = margin_values,
    estimate = numeric(length(margin_values)),
    std.error = numeric(length(margin_values)),
    conf.low = numeric(length(margin_values)),
    conf.high = numeric(length(margin_values))
  )
  
  for(j in 1:length(margin_values)) {
    estimates <- sapply(all_predictions, function(x) x$estimate[j])
    std_errors <- sapply(all_predictions, function(x) x$std.error[j])
    
    Q_bar <- mean(estimates, na.rm = TRUE)
    U_bar <- mean(std_errors^2, na.rm = TRUE)
    B <- var(estimates, na.rm = TRUE)
    m_obs <- sum(!is.na(estimates))
    T <- U_bar + (1 + 1/m_obs) * B
    
    pooled_results$estimate[j] <- Q_bar
    pooled_results$std.error[j] <- sqrt(T)
    pooled_results$conf.low[j] <- Q_bar - 1.96 * sqrt(T)
    pooled_results$conf.high[j] <- Q_bar + 1.96 * sqrt(T)
  }
  
  return(pooled_results)
}

# creating individual plots
create_panel_plot <- function(outcome, title) {
  
  margins_df <- run_mi_analysis_working(outcome)
  
  if(is.null(margins_df)) return(NULL)

  ggplot(margins_df, aes(x = pct_emps_psl_place, y = estimate)) +
    geom_point(size = 0.8, shape = 1, color = "black", stroke = 0.5) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 1, linewidth = 0.4) +
    scale_x_continuous(
      breaks = seq(0, 50, 10),
      labels = paste0(seq(0, 50, 10), "%"),
      expand = c(0.02, 0.02)
    ) +
    scale_y_continuous(
      breaks = seq(0.1, 0.7, 0.1),
      labels = paste0(seq(10, 70, 10), "%"),
      limits = c(0.1, 0.7),
      expand = c(0.02, 0.02)
    ) +
    labs(
      title = title,
      x = if(outcome %in% c("benefits_paidleave", "benefits_retirementplan", "benefits_tuition")) {
        "% of Firm's Workers Exposed to PSL Mandate"
      } else {
        ""
      },
      y = ""
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, hjust = 0.5),
      axis.title.x = element_text(size = 7),
      axis.title.y = element_text(size = 8),
      axis.text = element_text(size = 7),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.3)
    )
}

# panel structure
plots <- list()
for(i in 1:length(outcomes)) {
  plots[[i]] <- create_panel_plot(outcomes[i], titles[i])
}

combined_plot <- wrap_plots(plots, nrow = 2, ncol = 3)

# adding titles/labels
final_plot <- combined_plot + 
  plot_annotation(
    title = " ",
    caption = "Firm Exposure to PSL Mandate States or Counties",
    theme = theme(
      plot.caption = element_text(hjust = 0.5, size = 9),
      plot.title = element_text(size = 12)
    )
  )

final_plot_with_y <- wrap_elements(
  textGrob("% of Workers in Non-PSL Mandate States or Counties Reporting Access to Benefit", 
           rot = 90, gp = gpar(fontsize = 9))
) + final_plot + 
  plot_layout(widths = c(0.05, 0.95))


print(final_plot_with_y)

# save
ggsave(file.path(system_prefix, "results/exhibits/Appendix Figure 1.pdf"), final_plot_with_y, width = 8, height = 6)
