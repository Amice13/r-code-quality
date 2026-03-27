#=====================================================================================================================================#
#                                                                                                                                     #
# Title: "Americans’ Responses to COVID-19 and the Conditional Role of Dispositional Needs for Security: A Replication and Extension" #
# Authors: Adam R. Panish, Trent Ollerenshaw, & Joseph A. Vitriol                                                                     #
# Date: 9/21/2025                                                                                                                     #
#                                                                                                                                     #
########################### Code for generating Figures 1-3 in Main Text and Figure A6 in Appendix 6 ##################################
#                                                                                                                                     #
#=====================================================================================================================================#




#================================================================#
#                          Load Packages                         #
#================================================================#

library(readxl)
library(dplyr)
library(forcats)
library(gridExtra)
library(ltm)
library(Cairo)
library(ggplot2)
library(ggh4x)
library(ggtext)
library(patchwork)

#================================================================#
#                    Figure 1: Power Analyses                    #
#================================================================#

load("simulation results.RData")

fig1 <-
  ggplot(fig1.data, aes(`Effect Size`, Power, shape = sample)) +
  facet_wrap(~ dv) + 
  geom_hline(yintercept = c(.8, .9, 1), linetype = c(rep("ff", 9)), color = "grey65") +
  geom_line(linewidth = 0.5) +
  geom_point(fill = "white") +
  scale_shape_manual(values = c(22, 24, 21)) +
  scale_y_continuous(name='Power', 
                     breaks = c(0, .25, .5, .8, .9, 1),
                     labels = c("0%", "25%", "50%", "80%", "90%", "100%")) +
  scale_x_continuous(name = "Effect Size (\u03B2)", 
                     breaks = seq(.01, .2, by = .05)) +
  theme_bw() +
  theme(
    axis.ticks = element_line(color = "grey65"),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "grey65"),
    strip.background = element_rect(fill = "grey65", color = "grey65"),
    strip.text = element_text(color = "white")
  )

Cairo(3000, 1800, "Figure 1.tiff", res = 450)

fig1

dev.off()






#================================================================#
#                   Figure 2: Marginal Effects                   #
#================================================================#

# Load Results
fig2.data <- as.data.frame(read_excel("Marginal Effects for Figures.xlsx", sheet = "Main Models"))

# Prepare Results for Plotting
fig2.data[fig2.data$outcome == "concern",]$outcome <- "DV: COVID Concern"
fig2.data[fig2.data$outcome == "restrictions",]$outcome <- "DV: COVID Restrictions"
fig2.data[fig2.data$outcome == "behavior",]$outcome <- "DV: Health Behaviors"
fig2.data[fig2.data$measure == "auth",]$measure <- "Authoritarianism"
fig2.data[fig2.data$measure == "nsc",]$measure <- "Needs for Security\nand Certainty"
fig2.data[fig2.data$sample == "pooled",]$sample <- "Pooled Samples"

fig2.data$outcome <- factor(fig2.data$outcome, levels = c("DV: COVID Concern", "DV: COVID Restrictions", "DV: Health Behaviors"), ordered = T)
fig2.data$`Independent Variable` <- factor(fig2.data$measure, levels = c("Authoritarianism", "Needs for Security\nand Certainty"), ordered = T)

# Plot Results
fig2 <-
  ggplot(fig2.data, aes(x = engagement, y = estimate, group = `Independent Variable`, shape = `Independent Variable`)) +
  facet_wrap(~ outcome, ncol = 3) +
  geom_hline(yintercept = 0, linetype = "ff", color = "grey65") +
  geom_linerange(aes(ymin = estimate - 1.96*se, ymax = estimate + 1.96*se), 
                 position = position_dodge(0.8), color = "black", linewidth = 0.4) +
  geom_pointrange(aes(ymin = estimate - 1.372*se, ymax = estimate + 1.372*se), 
                  position = position_dodge(0.8), color = "black", fill = "white", linewidth = 0.7) +
  scale_shape_manual(values = c(19, 21)) +
  scale_x_continuous(name='Political Engagement', 
                     breaks = c(1, 3, 5), 
                     labels = c("Low", "Medium", "High")) +
  ylab("  Marginal Effect") +
  theme_bw() +
  theme(
    axis.ticks = element_line(color = "grey65"),
    legend.position = "bottom",
    panel.grid = element_blank(),
    panel.border = element_rect(color = "grey65"),
    strip.background = element_rect(fill = "grey65", color = "grey65"),
    strip.text = element_text(color = "white")
  )

Cairo(2500, 1500, "Figure 2.tiff", res = 400)

fig2

dev.off()






#================================================================#
#              Figure 3: Marginal Effects by Behavior            #
#================================================================#

# Load Results
fig3.data <- as.data.frame(read_excel("Marginal Effects for Figures.xlsx", sheet = "Individual Behaviors"))

# Prepare Results for Plotting
fig3.data$`Independent Variable` <- factor(ifelse(fig3.data$measure == "auth", "Authoritarianism", "Needs for Security\nand Certainty"), levels = c("Authoritarianism", "Needs for Security\nand Certainty"), ordered = T)
fig3.data$outcome <- factor(case_when(fig3.data$outcome == "masking" ~ "DV: Masking",
                                      fig3.data$outcome == "distancing" ~ "DV: Distancing",
                                      fig3.data$outcome == "vaccination" ~ "DV: Vaccination",
                                      fig3.data$outcome == "washhands" ~ "DV: Wash Hands",
                                      fig3.data$outcome == "dining" ~ "DV: Dining Out",
                                      fig3.data$outcome == "events" ~ "DV: Attend Events",
                                      fig3.data$outcome == "workhome" ~ "DV: Work from Home",
                                      fig3.data$outcome == "sanitize" ~ "DV: Sanitize Surfaces",
                                      fig3.data$outcome == "travel" ~ "DV: Travel"),
                            levels = c("DV: Masking", "DV: Distancing", "DV: Vaccination", "DV: Wash Hands", "DV: Dining Out", 
                                       "DV: Attend Events", "DV: Work from Home", "DV: Sanitize Surfaces", "DV: Travel"))

# Plot Results
fig3 <-
  ggplot(fig3.data, aes(x = engagement, y = estimate, group = `Independent Variable`, shape = `Independent Variable`)) +
  facet_wrap(~ outcome, ncol = 3) +
  geom_hline(yintercept = 0, linetype = "ff", color = "grey65") +
  geom_linerange(aes(ymin = estimate - 1.96*se, ymax = estimate + 1.96*se), 
                 position = position_dodge(0.6), color = "black", linewidth = 0.2) +
  geom_pointrange(aes(ymin = estimate - 1.372*se, ymax = estimate + 1.372*se), 
                  position = position_dodge(0.6), color = "black", fill = "white", linewidth = 0.7) +
  scale_shape_manual(values = c(19, 21)) +
  scale_x_continuous(name='Political Engagement', 
                     breaks = c(1, 3, 5), 
                     labels = c("Low", "Medium", "High")) +
  ylab("  Marginal Effect") +
  theme_bw() +
  theme(
    axis.ticks = element_line(color = "grey65"),
    axis.title.x = element_text(size = 12, face = "bold", vjust = -1),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    panel.border = element_rect(color = "grey65"),
    panel.grid = element_blank(),
    legend.position = "bottom",
    strip.background = element_rect(fill = "grey65", color = "grey65"),
    strip.text = element_text(color = "white", size = 11)
  )

Cairo(3500, 3500, "Figure 3.tiff", res = 450)

fig3

dev.off()



#================================================================#
#                  Figure A5: Binning Estimates                  #
#================================================================#

Binning_Estimates <- as.data.frame(read_excel("Marginal Effects for Figures.xlsx", sheet = "Binning Estimates"))

# Concern - Authoritarianism
linear_data <- subset(Binning_Estimates, outcome == "concern" & measure== "auth" & binned == "no")
binned_data <- subset(Binning_Estimates, outcome == "concern" & measure== "auth" & binned == "yes")

concern_auth_plot <- 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_line(data = linear_data, 
            aes(x = engagement, y = estimate), 
            color = "black", size = 1) +
  geom_ribbon(data = linear_data,
              aes(x = engagement, ymin = estimate - 1.96*se, ymax = estimate + 1.96*se), 
              alpha = 0.15, fill = "black") +
  geom_ribbon(data = linear_data,
              aes(x = engagement, ymin = estimate - 1.372*se, ymax = estimate + 1.372*se), 
              alpha = 0.25, fill = "black") +
  geom_point(data = binned_data,
             aes(x = engagement, y = estimate), 
             size = 3.5, color = "red") +  
  geom_errorbar(data = binned_data,
                aes(x = engagement, ymin = estimate - 1.96*se, ymax = estimate + 1.96*se),
                width = 0.03, color = "red", size = 5, alpha=0.15) +  # Increased size for thicker CI bars
  geom_errorbar(data = binned_data,
                aes(x = engagement, ymin = estimate - 1.372*se, ymax = estimate + 1.372*se),
                width = 0.03, color = "red", size = 5, alpha=0.30) +  # Increased size for thicker CI bars
  scale_x_continuous(name='Political Engagement', 
                     breaks = c(0,0.25,0.50,0.75, 1),
                     labels = c("Low", "", "", "", "High")) +
  scale_y_continuous(name='Marginal Effect', 
                     limits=c(-.32, .60), 
                     breaks=seq(-.25, .50, by=0.25),
                     labels = scales::label_comma(accuracy = 0.01)) +
  labs(title = "A. Concern (Authoritarianism)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 18, face="bold"),
    axis.line = element_line(size = 0.5, colour = "black"),
    axis.title.x = element_text(size = 13, face = "bold"),
    axis.title.y = element_text(size = 13, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

# Concern - NSC
linear_data <- subset(Binning_Estimates, outcome == "concern" & measure== "nsc" & binned == "no")
binned_data <- subset(Binning_Estimates, outcome == "concern" & measure== "nsc" & binned == "yes")

concern_nsc_plot <- 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_line(data = linear_data, 
            aes(x = engagement, y = estimate), 
            color = "black", size = 1) +
  geom_ribbon(data = linear_data,
              aes(x = engagement, ymin = estimate - 1.96*se, ymax = estimate + 1.96*se), 
              alpha = 0.15, fill = "black") +
  geom_ribbon(data = linear_data,
              aes(x = engagement, ymin = estimate - 1.372*se, ymax = estimate + 1.372*se), 
              alpha = 0.25, fill = "black") +
  geom_point(data = binned_data,
             aes(x = engagement, y = estimate), 
             size = 3.5, color = "red") +  
  geom_errorbar(data = binned_data,
                aes(x = engagement, ymin = estimate - 1.96*se, ymax = estimate + 1.96*se),
                width = 0.03, color = "red", size = 5, alpha=0.15) +  # Increased size for thicker CI bars
  geom_errorbar(data = binned_data,
                aes(x = engagement, ymin = estimate - 1.372*se, ymax = estimate + 1.372*se),
                width = 0.03, color = "red", size = 5, alpha=0.30) +  # Increased size for thicker CI bars
  scale_x_continuous(name='Political Engagement', 
                     breaks = c(0,0.25,0.50,0.75, 1),
                     labels = c("Low", "", "", "", "High")) +
  scale_y_continuous(name='Marginal Effect', 
                     limits=c(-.32, .60), 
                     breaks=seq(-.25, .50, by=0.25),
                     labels = scales::label_comma(accuracy = 0.01)) +
  labs(title = "B. Concern (Latent NSC)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 18, face="bold"),
    axis.line = element_line(size = 0.5, colour = "black"),
    axis.title.x = element_text(size = 13, face = "bold"),
    axis.title.y = element_text(size = 13, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

# Restrictions - Authoritarianism
linear_data <- subset(Binning_Estimates, outcome == "restrictions" & measure== "auth" & binned == "no")
binned_data <- subset(Binning_Estimates, outcome == "restrictions" & measure== "auth" & binned == "yes")

restrictions_auth_plot <- 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_line(data = linear_data, 
            aes(x = engagement, y = estimate), 
            color = "black", size = 1) +
  geom_ribbon(data = linear_data,
              aes(x = engagement, ymin = estimate - 1.96*se, ymax = estimate + 1.96*se), 
              alpha = 0.15, fill = "black") +
  geom_ribbon(data = linear_data,
              aes(x = engagement, ymin = estimate - 1.372*se, ymax = estimate + 1.372*se), 
              alpha = 0.25, fill = "black") +
  geom_point(data = binned_data,
             aes(x = engagement, y = estimate), 
             size = 3.5, color = "red") +  
  geom_errorbar(data = binned_data,
                aes(x = engagement, ymin = estimate - 1.96*se, ymax = estimate + 1.96*se),
                width = 0.03, color = "red", size = 5, alpha=0.15) +  # Increased size for thicker CI bars
  geom_errorbar(data = binned_data,
                aes(x = engagement, ymin = estimate - 1.372*se, ymax = estimate + 1.372*se),
                width = 0.03, color = "red", size = 5, alpha=0.30) +  # Increased size for thicker CI bars
  scale_x_continuous(name='Political Engagement', 
                     breaks = c(0,0.25,0.50,0.75, 1),
                     labels = c("Low", "", "", "", "High")) +
  scale_y_continuous(name='Marginal Effect', 
                     limits=c(-.32, .60), 
                     breaks=seq(-.25, .50, by=0.25),
                     labels = scales::label_comma(accuracy = 0.01)) +
  labs(title = "C. Restrictions (Authoritarianism)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 18, face="bold"),
    axis.line = element_line(size = 0.5, colour = "black"),
    axis.title.x = element_text(size = 13, face = "bold"),
    axis.title.y = element_text(size = 13, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

# Behavior - Authoritarianism
linear_data <- subset(Binning_Estimates, outcome == "behavior" & measure== "auth" & binned == "no")
binned_data <- subset(Binning_Estimates, outcome == "behavior" & measure== "auth" & binned == "yes")

behavior_auth_plot <- 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_line(data = linear_data, 
            aes(x = engagement, y = estimate), 
            color = "black", size = 1) +
  geom_ribbon(data = linear_data,
              aes(x = engagement, ymin = estimate - 1.96*se, ymax = estimate + 1.96*se), 
              alpha = 0.15, fill = "black") +
  geom_ribbon(data = linear_data,
              aes(x = engagement, ymin = estimate - 1.372*se, ymax = estimate + 1.372*se), 
              alpha = 0.25, fill = "black") +
  geom_point(data = binned_data,
             aes(x = engagement, y = estimate), 
             size = 3.5, color = "red") +  
  geom_errorbar(data = binned_data,
                aes(x = engagement, ymin = estimate - 1.96*se, ymax = estimate + 1.96*se),
                width = 0.03, color = "red", size = 5, alpha=0.15) +  # Increased size for thicker CI bars
  geom_errorbar(data = binned_data,
                aes(x = engagement, ymin = estimate - 1.372*se, ymax = estimate + 1.372*se),
                width = 0.03, color = "red", size = 5, alpha=0.30) +  # Increased size for thicker CI bars
  scale_x_continuous(name='Political Engagement', 
                     breaks = c(0,0.25,0.50,0.75, 1),
                     labels = c("Low", "", "", "", "High")) +
  scale_y_continuous(name='Marginal Effect', 
                     limits=c(-.32, .60), 
                     breaks=seq(-.25, .50, by=0.25),
                     labels = scales::label_comma(accuracy = 0.01)) +
  labs(title = "D. Behavior (Authoritarianism)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 18, face="bold"),
    axis.line = element_line(size = 0.5, colour = "black"),
    axis.title.x = element_text(size = 13, face = "bold"),
    axis.title.y = element_text(size = 13, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

# Behavior - NSC
linear_data <- subset(Binning_Estimates, outcome == "behavior" & measure== "nsc" & binned == "no")
binned_data <- subset(Binning_Estimates, outcome == "behavior" & measure== "nsc" & binned == "yes")

behavior_nsc_plot <- 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_line(data = linear_data, 
            aes(x = engagement, y = estimate), 
            color = "black", size = 1) +
  geom_ribbon(data = linear_data,
              aes(x = engagement, ymin = estimate - 1.96*se, ymax = estimate + 1.96*se), 
              alpha = 0.15, fill = "black") +
  geom_ribbon(data = linear_data,
              aes(x = engagement, ymin = estimate - 1.372*se, ymax = estimate + 1.372*se), 
              alpha = 0.25, fill = "black") +
  geom_point(data = binned_data,
             aes(x = engagement, y = estimate), 
             size = 3.5, color = "red") +  
  geom_errorbar(data = binned_data,
                aes(x = engagement, ymin = estimate - 1.96*se, ymax = estimate + 1.96*se),
                width = 0.03, color = "red", size = 5, alpha=0.15) +  # Increased size for thicker CI bars
  geom_errorbar(data = binned_data,
                aes(x = engagement, ymin = estimate - 1.372*se, ymax = estimate + 1.372*se),
                width = 0.03, color = "red", size = 5, alpha=0.30) +  # Increased size for thicker CI bars
  scale_x_continuous(name='Political Engagement', 
                     breaks = c(0,0.25,0.50,0.75, 1),
                     labels = c("Low", "", "", "", "High")) +
  scale_y_continuous(name='Marginal Effect', 
                     limits=c(-.32, .60), 
                     breaks=seq(-.25, .50, by=0.25),
                     labels = scales::label_comma(accuracy = 0.01)) +
  labs(title = "E. Behavior (Latent NSC)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 18, face="bold"),
    axis.line = element_line(size = 0.5, colour = "black"),
    axis.title.x = element_text(size = 13, face = "bold"),
    axis.title.y = element_text(size = 13, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

Cairo(4500, 4500, "Figure App 5.tiff", res = 450)

(concern_auth_plot | concern_nsc_plot) /
  (restrictions_auth_plot | plot_spacer()) /
  (behavior_auth_plot + behavior_nsc_plot) + plot_layout()

dev.off()




#================================================================#
#      Figure A6: Marginal Effects Predicting PID & Ideology     #
#================================================================#

# Load Results
figapp6.data <- as.data.frame(read_excel("Marginal Effects for Figures.xlsx", sheet = "PID-IDEO"))

# Prepare Results for Plotting
figapp6.data$`Independent Variable` <- factor(ifelse(figapp6.data$measure == "auth", "Authoritarianism", "Needs for Security\nand Certainty"), 
                                              levels = c("Authoritarianism", "Needs for Security\nand Certainty"), ordered = T)
figapp6.data$outcome <- factor(case_when(figapp6.data$outcome == "pid" ~ "DV: Republican Party ID",
                                      figapp6.data$outcome == "ideo" ~ "DV: Conservative Ideology",
                                      figapp6.data$outcome == "leftright" ~ "DV: Right-Wing Composite"),
                            levels = c("DV: Republican Party ID", "DV: Conservative Ideology", "DV: Right-Wing Composite"))

# Plot Results
app6.fig <-
  ggplot(figapp6.data, aes(x = engagement, y = coef, group = `Independent Variable`, shape = `Independent Variable`)) +
  facet_wrap(~ outcome, ncol = 3) +
  geom_hline(yintercept = 0, linetype = "ff", color = "grey65") +
  geom_linerange(aes(ymin = coef - 1.96*se, ymax = coef + 1.96*se), 
                 position = position_dodge(0.8), color = "black", linewidth = 0.4) +
  geom_pointrange(aes(ymin = coef - 1.372*se, ymax = coef + 1.372*se), 
                  position = position_dodge(0.8), color = "black", fill = "white", linewidth = 0.7) +
  scale_shape_manual(values = c(19, 21)) +
  scale_x_continuous(name='Political Engagement', 
                     breaks = c(1, 3, 5), 
                     labels = c("Low", "Medium", "High")) +
  ylab("  Marginal Effect") +
  theme_bw() +
  theme(
    axis.ticks = element_line(color = "grey65"),
    legend.position = "bottom",
    panel.grid = element_blank(),
    panel.border = element_rect(color = "grey65"),
    strip.background = element_rect(fill = "grey65", color = "grey65"),
    strip.text = element_text(color = "white")
  )

Cairo(3500, 2000, "Figure App 6.tiff", res = 450)

app6.fig

dev.off()
