# Replication Files for Sacred Speech: Analyzing the Influence of Congressional Leadership on Religious Rhetoric

# 05_event_study_plots
# This file takes the "speeches" and "dcinbox" CSVs 
# It includes code to replicate Figure 3 in the paper 

# Last updated July 18, 2024

# Initial settings -------------------------------------------------------------
my_packages <- c("ggplot2", "dplyr",  "ggpubr") 
lapply(my_packages, require, character.only = TRUE) 

# Upload the data --------------------------------------------------------------
{df <- read.csv("R Data/speeches.csv")
df_dc <- read.csv("R Data/dcinbox.csv")}


# Setting up the data ----------------------------------------------------------
{# Creating start/stop dates (2 months before/after Oct 25, 2023 & Oct 25, 2022)
  start_date <- as.Date("8/25/23", format = "%m/%d/%y")
  stop_date <- as.Date("12/25/23", format = "%m/%d/%y")
  start_date_22 <- as.Date("8/25/22", format = "%m/%d/%y")
  stop_date_22 <- as.Date("12/25/22", format = "%m/%d/%y")
  
  # Subsetting data to start/stop dates: Speeches
  did_data = df[df$full_dates >= start_date & df$full_dates <= stop_date ,]
  did_data_22 = df[df$full_dates >= start_date_22 & df$full_dates <= stop_date_22 ,]
  
  # Remove Johnson's speech
  did_data <- did_data[!(did_data$date == "10/25/23" & did_data$bioguide_id=="J000299"),]

  # Subsetting data to start/stop dates: Newsletters
  did_data_dc = df_dc[df_dc$full_dates >= start_date & df_dc$full_dates <= stop_date ,]
  did_data_dc_22 = df_dc[df_dc$full_dates >= start_date_22 & df_dc$full_dates <= stop_date_22 ,]
  }


# Figure 3: Average Speeches/Newsletters ---------------------------------------

# Making Plot Data: Speeches 2023
gg.data <-
  did_data %>%
  mutate(
    treat = factor(rep, levels = c(0, 1), labels = c('Democrat', 'Republican')), 
    after = factor(j_speech, levels = c(0, 1), labels = c('Before Johnson\n8/25/23 - 10/24/23', 'After Johnson\n10/25/23 - 12/25/23'))
  ) %>%
  group_by(treat,after) %>%
  ## compute mean and CI of the results
  summarise(
    results.mean = mean(all_terms_binary),
    results.lower = mean(all_terms_binary) - sd(all_terms_binary)/sqrt(length(all_terms_binary))*2,
    results.upper = mean(all_terms_binary) + sd(all_terms_binary)/sqrt(length(all_terms_binary))*2,
    .groups = 'drop'
  )

# Making Plot: Speeches 2023
p <- ggplot(data = gg.data[!is.na(gg.data$treat),],
            aes(x = after, y = results.mean, 
                linetype = treat))

speech_plot <- p +
  geom_line(aes(group = treat, linetype = treat)) +
  geom_errorbar(aes(ymin = results.lower, ymax = results.upper), width = 0.02) +
  scale_linetype_manual(values = c("solid","dashed")) +
  theme_bw() +
  labs(x = "", y="", title = "House Speeches, 2023") +
  scale_color_manual(values = c("black","black"),
                     guide = guide_legend(override.aes = list(
                       shape = NA,
                       linetype = c("solid", "dashed")))) +
  theme(legend.position = c(0.01, 0.99),
        legend.justification = c("left", "top"),
        legend.text = element_text(size = 8),
        legend.background = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12, hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA)) +
  ylim(0, 0.4)


# Making Plot Data; Newsletters 2023
gg.data_dc <-
  did_data_dc %>%
  mutate(
    treat = factor(rep, levels = c(0, 1), labels = c('Democrat', 'Republican')), 
    after = factor(j_speech, levels = c(0, 1), labels = c('Before Johnson\n8/25/23 - 10/24/23', 'After Johnson\n10/25/23 - 12/25/23'))
  ) %>%
  group_by(treat, after) %>%
  ## compute mean and CI of the results
  summarise(
    results.mean = mean(all_terms_binary),
    results.lower = mean(all_terms_binary) - sd(all_terms_binary)/sqrt(length(all_terms_binary))*2,
    results.upper = mean(all_terms_binary) + sd(all_terms_binary)/sqrt(length(all_terms_binary))*2,
    .groups = 'drop'
  )

# Make plot; Newsletters 2023
p <- ggplot(data = gg.data_dc,
            aes(x = after, y = results.mean, 
                linetype = treat))

newsletter_plot <- p +
  geom_line(aes(group = treat, linetype = treat)) +
  geom_errorbar(aes(ymin = results.lower, ymax = results.upper), width = 0.02) +
  scale_linetype_manual(values = c("solid","dashed")) +
  theme_bw() +
  labs(x = "", y="", title = "Newsletters, 2023") +
  scale_color_manual(values = c("black","black"),
                     guide = guide_legend(override.aes = list(
                       shape = NA,
                       linetype = c("solid", "dashed")))) +  theme(legend.position = "none")  +
  ylim(0,0.4) +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA))



# Making Plot Data; Speeches 2022
gg.data <-
  did_data_22 %>%
  mutate(
    treat = factor(rep, levels = c(0, 1), labels = c('Democrat', 'Republican')), 
    after = factor(j_speech_22, levels = c(0, 1), labels = c('8/25/22 - 10/24/22', '10/25/22 - 12/25/22'))
  ) %>%
  group_by(treat, after) %>%
  ## compute mean and CI of the results
  summarise(
    results.mean = mean(all_terms_binary),
    results.lower = mean(all_terms_binary) - sd(all_terms_binary)/sqrt(length(all_terms_binary))*2,
    results.upper = mean(all_terms_binary) + sd(all_terms_binary)/sqrt(length(all_terms_binary))*2,
    .groups = 'drop'
  )

# Making Plot; Speeches 2022
p <- ggplot(data = gg.data[!is.na(gg.data$treat),],
            aes(x = after, y = results.mean, 
                linetype = treat))

speech_plot_22 <- p +
  geom_line(aes(group = treat, linetype = treat)) +
  geom_errorbar(aes(ymin = results.lower, ymax = results.upper), width = 0.02) +
  scale_linetype_manual(values = c("solid","dashed")) +
  theme_bw() +
  labs(x = "", y="", title = "House Speeches, 2022") +
  scale_color_manual(values = c("black","black"),
                     guide = guide_legend(override.aes = list(
                       shape = NA,
                       linetype = c("solid", "dashed")))) +
  theme(legend.position = c(0.01, 0.99),
        legend.justification = c("left", "top"),
        legend.text = element_text(size = 8),
        legend.background = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12, hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA)) +
  ylim(0, 0.4)


# Making Plot Data; Newsletters 2022
gg.data_dc <-
  did_data_dc_22 %>%
  mutate(
    treat = factor(rep, levels = c(0, 1), labels = c('Democrat', 'Republican')), 
    after = factor(j_speech_22, levels = c(0, 1), labels = c('8/25/22 - 10/24/22', '10/25/22 - 12/25/22'))
  ) %>%
  group_by(treat, after) %>%
  ## compute mean and CI of the results
  summarise(
    results.mean = mean(all_terms_binary),
    results.lower = mean(all_terms_binary) - sd(all_terms_binary)/sqrt(length(all_terms_binary))*2,
    results.upper = mean(all_terms_binary) + sd(all_terms_binary)/sqrt(length(all_terms_binary))*2,
    .groups = 'drop'
  )

# Making the plot; Newsletters 2022
p <- ggplot(data = gg.data_dc,
            aes(x = after, y = results.mean, 
                linetype = treat))

newsletter_plot_22 <- p +
  geom_line(aes(group = treat, linetype = treat)) +
  geom_errorbar(aes(ymin = results.lower, ymax = results.upper), width = 0.02) +
  scale_linetype_manual(values = c("solid","dashed")) +
  theme_bw() +
  labs(x = "", y="", title = "Newsletters, 2022") +
  scale_color_manual(values = c("black","black"),
                     guide = guide_legend(override.aes = list(
                       shape = NA,
                       linetype = c("solid", "dashed")))) +  theme(legend.position = "none")  +
  ylim(0,0.4) +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA))



# Putting the four plots together
figure <- ggarrange(speech_plot, newsletter_plot, speech_plot_22, newsletter_plot_22)

annotate_figure(figure, left = "Average Number of Religious Speeches/Newsletters")

