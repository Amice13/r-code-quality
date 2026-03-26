#########GRAPH treatment effects ####################
#1. ATE by time and experiment
#2. HTE by previous health


# Load required packages
library(ggplot2)
library(dplyr)
library(readxl)
library(gridExtra)
library(ggsignif)
library(grid)
library(ggpubr)
library(extrafont)
library(openxlsx)

library(pdftools)
library(magick)

font_import()
loadfonts()

# Clear the environment
rm(list = ls())

#Loader ATE ind:
ATE_table <- read_excel("~/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Table S2 (graph).xlsx")

##############################################
###########N05 (DDD)################
###############################################Laver lang table til at plotte Effekterne:
N05 <- rbind(c("N05","First 12 months","Social assistance",ATE_table$ATE...2[4],ATE_table$SE...3[4]))
N05 <- rbind(N05,c("N05","12 to 60 months","Social assistance",ATE_table$ATE...5[4],ATE_table$SE...6[4]))
N05 <- rbind(N05,c("N05","60 to 120 months","Social assistance",ATE_table$ATE...8[4],ATE_table$SE...9[4]))

N05 <- rbind(N05, c("N05","First 12 months","Unemployment benefits 1",ATE_table$ATE...2[10],ATE_table$SE...3[10]))
N05 <- rbind(N05, c("N05","12 to 60 months","Unemployment benefits 1",ATE_table$ATE...5[10],ATE_table$SE...6[10]))
N05 <- rbind(N05, c("N05","60 to 120 months","Unemployment benefits 1",ATE_table$ATE...8[10],ATE_table$SE...9[10]))

N05 <- rbind(N05, c("N05","First 12 months","Unemployment benefits 2a",ATE_table$ATE...2[16],ATE_table$SE...3[16]))
N05 <- rbind(N05, c("N05","12 to 60 months","Unemployment benefits 2a",ATE_table$ATE...5[16],ATE_table$SE...6[16]))
N05 <- rbind(N05, c("N05","60 to 120 months","Unemployment benefits 2a",ATE_table$ATE...8[16],ATE_table$SE...9[16]))

N05 <- rbind(N05, c("N05","First 12 months","Unemployment benefits 2b",ATE_table$ATE...2[22],ATE_table$SE...3[22]))
N05 <- rbind(N05, c("N05","12 to 60 months","Unemployment benefits 2b",ATE_table$ATE...5[22],ATE_table$SE...6[22]))
N05 <- rbind(N05, c("N05","60 to 120 months","Unemployment benefits 2b",ATE_table$ATE...8[22],ATE_table$SE...9[22]))

N05 <- rbind(N05, c("N05","First 12 months","Unemployment benefits 2c",ATE_table$ATE...2[28],ATE_table$SE...3[28]))
N05 <- rbind(N05, c("N05","12 to 60 months","Unemployment benefits 2c",ATE_table$ATE...5[28],ATE_table$SE...6[28]))
N05 <- rbind(N05, c("N05","60 to 120 months","Unemployment benefits 2c",ATE_table$ATE...8[28],ATE_table$SE...9[28]))

#laver til data.frame:
N05 = data.frame(N05)
N05$X4 <- as.numeric(N05$X4)
N05$X5 <- as.numeric(N05$X5)


# Naming the columns using colnames()
colnames(N05) <- c("Variable", "Time", "Experiment","ATE","SE")
N05$Time <- factor(N05$Time, levels = c("First 12 months", "12 to 60 months", "60 to 120 months"))



# Create the plot for Main text:
filtered_data <- N05[N05$Experiment %in% c("Social assistance", "Unemployment benefits 1"), ]
plot <- ggplot(filtered_data, aes(x = Experiment, y = ATE, fill = Time)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = ATE - 1.96*SE, ymax = ATE + 1.96*SE), position = position_dodge(width = 0.9), width = 0.2) +
  labs(x = "", y = "Defined Daily Doses (DDD)") +
  scale_fill_grey(start = 0.5, end = 0.9) +
  scale_y_continuous(limits = c(-31, 25), breaks = seq(-30, 25, by = 10)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5, vjust = 0.5),  # Adjust size here
    axis.text.y = element_text(size = 16),  # Adjust size here
    axis.title.x = element_text(size = 16),  # Adjust size here
    axis.title.y = element_text(size = 16),  # Adjust size here
    legend.text = element_text(size = 17),  # Adjust size here
    legend.title = element_blank(),  # Adjust size here
  )
# Save the plot as a PDF file
ggsave("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/N05_ATE_main.pdf", plot, width = 8, height = 6, units = "in", dpi = 600)

# Create the plot for Appendix:
filtered_data <- N05[N05$Experiment %in% c("Unemployment benefits 2a", "Unemployment benefits 2b","Unemployment benefits 2c"), ]
plot <- ggplot(filtered_data, aes(x = Experiment, y = ATE, fill = Time)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = ATE - 1.96*SE, ymax = ATE + 1.96*SE), position = position_dodge(width = 0.9), width = 0.2) +
  labs(x = "", y = "Defined Daily Doses (DDD)") +
  scale_fill_grey() +
  scale_fill_grey(start = 0.5, end = 0.9) +
  scale_y_continuous(limits = c(-31, 25), breaks = seq(-30, 25, by = 10)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 16, angle = 0, hjust = 0.5, vjust = 0.5),  # Adjust size here
    axis.text.y = element_text(size = 16),  # Adjust size here
    axis.title.x = element_text(size = 16),  # Adjust size here
    axis.title.y = element_text(size = 16),  # Adjust size here
    legend.text = element_text(size = 17),  # Adjust size here
    legend.title = element_blank(),  # Adjust size here
  )
# Save the plot as a PDF file
ggsave("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/N05_ATE_appendix.pdf", plot, width = 10, height = 6, units = "in", dpi = 600)





##############################################
  ###########N06 (DDD)################
##############################################
#Laver lang table til at plotte Effekterne:
N06 <- rbind(c("N06","First 12 months","Social assistance",ATE_table$ATE...2[5],ATE_table$SE...3[5]))
N06 <- rbind(N06,c("N06","12 to 60 months","Social assistance",ATE_table$ATE...5[5],ATE_table$SE...6[5]))
N06 <- rbind(N06,c("N06","60 to 120 months","Social assistance",ATE_table$ATE...8[5],ATE_table$SE...9[5]))

N06 <- rbind(N06, c("N06","First 12 months","Unemployment benefits 1",ATE_table$ATE...2[11],ATE_table$SE...3[11]))
N06 <- rbind(N06, c("N06","12 to 60 months","Unemployment benefits 1",ATE_table$ATE...5[11],ATE_table$SE...6[11]))
N06 <- rbind(N06, c("N06","60 to 120 months","Unemployment benefits 1",ATE_table$ATE...8[11],ATE_table$SE...9[11]))

N06 <- rbind(N06, c("N06","First 12 months","Unemployment benefits 2a",ATE_table$ATE...2[17],ATE_table$SE...3[17]))
N06 <- rbind(N06, c("N06","12 to 60 months","Unemployment benefits 2a",ATE_table$ATE...5[17],ATE_table$SE...6[17]))
N06 <- rbind(N06, c("N06","60 to 120 months","Unemployment benefits 2a",ATE_table$ATE...8[17],ATE_table$SE...9[17]))

N06 <- rbind(N06, c("N06","First 12 months","Unemployment benefits 2b",ATE_table$ATE...2[23],ATE_table$SE...3[23]))
N06 <- rbind(N06, c("N06","12 to 60 months","Unemployment benefits 2b",ATE_table$ATE...5[23],ATE_table$SE...6[23]))
N06 <- rbind(N06, c("N06","60 to 120 months","Unemployment benefits 2b",ATE_table$ATE...8[23],ATE_table$SE...9[23]))

N06 <- rbind(N06, c("N06","First 12 months","Unemployment benefits 2c",ATE_table$ATE...2[29],ATE_table$SE...3[29]))
N06 <- rbind(N06, c("N06","12 to 60 months","Unemployment benefits 2c",ATE_table$ATE...5[29],ATE_table$SE...6[29]))
N06 <- rbind(N06, c("N06","60 to 120 months","Unemployment benefits 2c",ATE_table$ATE...8[29],ATE_table$SE...9[29]))

#laver til data.frame:
N06 = data.frame(N06)
N06$X4 <- as.numeric(N06$X4)
N06$X5 <- as.numeric(N06$X5)

# Naming the columns using colnames()
colnames(N06) <- c("Variable", "Time", "Experiment","ATE","SE")
N06$Time <- factor(N06$Time, levels = c("First 12 months", "12 to 60 months", "60 to 120 months"))


# Create the plot for Main text:
filtered_data <- N06[N06$Experiment %in% c("Social assistance", "Unemployment benefits 1"), ]
plot <- ggplot(filtered_data, aes(x = Experiment, y = ATE, fill = Time)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = ATE - 1.96*SE, ymax = ATE + 1.96*SE), position = position_dodge(width = 0.9), width = 0.2) +
  labs(x = "", y = "Defined Daily Doses (DDD)") +
  scale_fill_grey() +
  scale_fill_grey(start = 0.5, end = 0.9) +
  scale_y_continuous(limits = c(-31, 25), breaks = seq(-30, 25, by = 10)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5, vjust = 0.5),  # Adjust size here
    axis.text.y = element_text(size = 16),  # Adjust size here
    axis.title.x = element_text(size = 16),  # Adjust size here
    axis.title.y = element_text(size = 16),  # Adjust size here
    legend.text = element_text(size = 17),  # Adjust size here
    legend.title = element_blank(),  # Adjust size here
  )
# Save the plot as a PDF file
ggsave("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/N06_ATE_main.pdf", plot, width = 8, height = 6, units = "in", dpi = 600)

# Create the plot for Appendix:
filtered_data <- N06[N06$Experiment %in% c("Unemployment benefits 2a", "Unemployment benefits 2b","Unemployment benefits 2c"), ]
plot <- ggplot(filtered_data, aes(x = Experiment, y = ATE, fill = Time)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = ATE - 1.96*SE, ymax = ATE + 1.96*SE), position = position_dodge(width = 0.9), width = 0.2) +
  labs(x = "", y = "Defined Daily Doses (DDD)") +
  scale_fill_grey() +
  scale_fill_grey(start = 0.5, end = 0.9) +
  scale_y_continuous(limits = c(-31, 25), breaks = seq(-30, 25, by = 10)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 16, angle = 0, hjust = 0.5, vjust = 0.5),  # Adjust size here
    axis.text.y = element_text(size = 16),  # Adjust size here
    axis.title.x = element_text(size = 16),  # Adjust size here
    axis.title.y = element_text(size = 16),  # Adjust size here
    legend.text = element_text(size = 17),  # Adjust size here
    legend.title = element_blank(),  # Adjust size here
  )
# Save the plot as a PDF file
ggsave("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/N06_ATE_appendix.pdf", plot, width = 10, height = 6, units = "in", dpi = 600)





################################################
########### Earnings (dollars) ################
################################################
#Laver lang table til at plotte Effekterne:
Earnings <- rbind(c("Earnings","First 12 months","Social assistance",ATE_table$ATE...2[2],ATE_table$SE...3[2]))
Earnings <- rbind(Earnings, c("Earnings","12 to 60 months","Social assistance",ATE_table$ATE...5[2],ATE_table$SE...6[2]))
Earnings <- rbind(Earnings, c("Earnings","60 to 120 months","Social assistance",ATE_table$ATE...8[2],ATE_table$SE...9[2]))

Earnings <- rbind(Earnings, c("Earnings","First 12 months","Unemployment benefits 1",ATE_table$ATE...2[8],ATE_table$SE...3[8]))
Earnings <- rbind(Earnings, c("Earnings","12 to 60 months","Unemployment benefits 1",ATE_table$ATE...5[8],ATE_table$SE...6[8]))
Earnings <- rbind(Earnings, c("Earnings","60 to 120 months","Unemployment benefits 1",ATE_table$ATE...8[8],ATE_table$SE...9[8]))

Earnings <- rbind(Earnings, c("Earnings","First 12 months","Unemployment benefits 2a",ATE_table$ATE...2[14],ATE_table$SE...3[14]))
Earnings <- rbind(Earnings, c("Earnings","12 to 60 months","Unemployment benefits 2a",ATE_table$ATE...5[14],ATE_table$SE...6[14]))
Earnings <- rbind(Earnings, c("Earnings","60 to 120 months","Unemployment benefits 2a",ATE_table$ATE...8[14],ATE_table$SE...9[14]))

Earnings <- rbind(Earnings, c("Earnings","First 12 months","Unemployment benefits 2b",ATE_table$ATE...2[20],ATE_table$SE...3[20]))
Earnings <- rbind(Earnings, c("Earnings","12 to 60 months","Unemployment benefits 2b",ATE_table$ATE...5[20],ATE_table$SE...6[20]))
Earnings <- rbind(Earnings, c("Earnings","60 to 120 months","Unemployment benefits 2b",ATE_table$ATE...8[20],ATE_table$SE...9[20]))

Earnings <- rbind(Earnings, c("Earnings","First 12 months","Unemployment benefits 2c",ATE_table$ATE...2[26],ATE_table$SE...3[26]))
Earnings <- rbind(Earnings, c("Earnings","12 to 60 months","Unemployment benefits 2c",ATE_table$ATE...5[26],ATE_table$SE...6[26]))
Earnings <- rbind(Earnings, c("Earnings","60 to 120 months","Unemployment benefits 2c",ATE_table$ATE...8[26],ATE_table$SE...9[26]))

#laver til data.frame:
Earnings = data.frame(Earnings)
Earnings$X4 <- as.numeric(Earnings$X4)
Earnings$X5 <- as.numeric(Earnings$X5)

# Naming the columns using colnames()
colnames(Earnings) <- c("Variable", "Time", "Experiment","ATE","SE")
Earnings$Time <- factor(Earnings$Time, levels = c("First 12 months", "12 to 60 months", "60 to 120 months"))


# Create the plot for Main text:
filtered_data <- Earnings[Earnings$Experiment %in% c("Social assistance", "Unemployment benefits 1"), ]
plot <- ggplot(filtered_data, aes(x = Experiment, y = ATE, fill = Time)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = ATE - 1.96*SE, ymax = ATE + 1.96*SE), position = position_dodge(width = 0.9), width = 0.2) +
  labs(x = "", y = "US Dollars") +
  scale_fill_grey() +
  scale_fill_grey(start = 0.5, end = 0.9) +
  theme_minimal() +
  scale_y_continuous(limits = c(-3000, 5000), breaks = seq(-3000, 5000, by = 1000)) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5, vjust = 0.5),  # Adjust size here
    axis.text.y = element_text(size = 16),  # Adjust size here
    axis.title.x = element_text(size = 16),  # Adjust size here
    axis.title.y = element_text(size = 16),  # Adjust size here
    legend.text = element_text(size = 17),  # Adjust size here
    legend.title = element_blank(),  # Adjust size here
  )# Save the plot as a PDF file
ggsave("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/Earnings_ATE_main.pdf", plot, width = 8, height = 6, units = "in", dpi = 600)

# Create the plot for Appendix:
filtered_data <- Earnings[Earnings$Experiment %in% c("Unemployment benefits 2a", "Unemployment benefits 2b","Unemployment benefits 2c"), ]
plot <- ggplot(filtered_data, aes(x = Experiment, y = ATE, fill = Time)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = ATE - 1.96*SE, ymax = ATE + 1.96*SE), position = position_dodge(width = 0.9), width = 0.2) +
  labs(x = "", y = "US Dollars") +
  scale_fill_grey() +
  scale_fill_grey(start = 0.5, end = 0.9) +
  theme_minimal() +
  scale_y_continuous(limits = c(-3000, 5000), breaks = seq(-3000, 5000, by = 1000)) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 16, angle = 0, hjust = 0.5, vjust = 0.5),  # Adjust size here
    axis.text.y = element_text(size = 16),  # Adjust size here
    axis.title.x = element_text(size = 16),  # Adjust size here
    axis.title.y = element_text(size = 16),  # Adjust size here
    legend.text = element_text(size = 17),  # Adjust size here
    legend.title = element_blank(),  # Adjust size here
  )
# Save the plot as a PDF file
ggsave("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/Earnings_ATE_appendix.pdf", plot, width = 10, height = 6, units = "in", dpi = 600)






################################################
########### Self-sufficient (weeks) ################
################################################
#Laver lang table til at plotte Effekterne:
Self_sufficient <- rbind(c("Self-sufficient","First 12 months","Social assistance",ATE_table$ATE...2[3],ATE_table$SE...3[3]))
Self_sufficient <- rbind(Self_sufficient, c("Self-sufficient","12 to 60 months","Social assistance",ATE_table$ATE...5[3],ATE_table$SE...6[3]))
Self_sufficient <- rbind(Self_sufficient, c("Self-sufficient","60 to 120 months","Social assistance",ATE_table$ATE...8[3],ATE_table$SE...9[3]))

Self_sufficient <- rbind(Self_sufficient, c("Self-sufficient","First 12 months","Unemployment benefits 1",ATE_table$ATE...2[9],ATE_table$SE...3[9]))
Self_sufficient <- rbind(Self_sufficient, c("Self-sufficient","12 to 60 months","Unemployment benefits 1",ATE_table$ATE...5[9],ATE_table$SE...6[9]))
Self_sufficient <- rbind(Self_sufficient, c("Self-sufficient","60 to 120 months","Unemployment benefits 1",ATE_table$ATE...8[9],ATE_table$SE...9[9]))

Self_sufficient <- rbind(Self_sufficient, c("Self-sufficient","First 12 months","Unemployment benefits 2a",ATE_table$ATE...2[15],ATE_table$SE...3[15]))
Self_sufficient <- rbind(Self_sufficient, c("Self-sufficient","12 to 60 months","Unemployment benefits 2a",ATE_table$ATE...5[15],ATE_table$SE...6[15]))
Self_sufficient <- rbind(Self_sufficient, c("Self-sufficient","60 to 120 months","Unemployment benefits 2a",ATE_table$ATE...8[15],ATE_table$SE...9[15]))

Self_sufficient <- rbind(Self_sufficient, c("Self-sufficient","First 12 months","Unemployment benefits 2b",ATE_table$ATE...2[21],ATE_table$SE...3[21]))
Self_sufficient <- rbind(Self_sufficient, c("Self-sufficient","12 to 60 months","Unemployment benefits 2b",ATE_table$ATE...5[21],ATE_table$SE...6[21]))
Self_sufficient <- rbind(Self_sufficient, c("Self-sufficient","60 to 120 months","Unemployment benefits 2b",ATE_table$ATE...8[21],ATE_table$SE...9[21]))

Self_sufficient <- rbind(Self_sufficient, c("Self-sufficient","First 12 months","Unemployment benefits 2c",ATE_table$ATE...2[27],ATE_table$SE...3[27]))
Self_sufficient <- rbind(Self_sufficient, c("Self-sufficient","12 to 60 months","Unemployment benefits 2c",ATE_table$ATE...5[27],ATE_table$SE...6[27]))
Self_sufficient <- rbind(Self_sufficient, c("Self-sufficient","60 to 120 months","Unemployment benefits 2c",ATE_table$ATE...8[27],ATE_table$SE...9[27]))

#laver til data.frame:
Self_sufficient = data.frame(Self_sufficient)
Self_sufficient$X4 <- as.numeric(Self_sufficient$X4)
Self_sufficient$X5 <- as.numeric(Self_sufficient$X5)

# Naming the columns using colnames()
colnames(Self_sufficient) <- c("Variable", "Time", "Experiment","ATE","SE")
Self_sufficient$Time <- factor(Self_sufficient$Time, levels = c("First 12 months", "12 to 60 months", "60 to 120 months"))

# Create the plot for Main text:
filtered_data <- Self_sufficient[Self_sufficient$Experiment %in% c("Social assistance", "Unemployment benefits 1"), ]
plot <- ggplot(filtered_data, aes(x = Experiment, y = ATE, fill = Time)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = ATE - 1.96*SE, ymax = ATE + 1.96*SE), position = position_dodge(width = 0.9), width = 0.2) +
  labs(x = "", y = "Weeks") +
  scale_fill_grey() +
  scale_fill_grey(start = 0.5, end = 0.9) +
  theme_minimal() +
  scale_y_continuous(limits = c(-2, 4), breaks = seq(-2, 3.5, by = 1)) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5, vjust = 0.5),  # Adjust size here
    axis.text.y = element_text(size = 16),  # Adjust size here
    axis.title.x = element_text(size = 16),  # Adjust size here
    axis.title.y = element_text(size = 16),  # Adjust size here
    legend.text = element_text(size = 17),  # Adjust size here
    legend.title = element_blank(),  # Adjust size here
  )
# Save the plot as a PDF file
ggsave("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/Self_sufficient_ATE_main.pdf", plot, width = 8, height = 6, units = "in", dpi = 600)

# Create the plot for Appendix:
filtered_data <- Self_sufficient[Self_sufficient$Experiment %in% c("Unemployment benefits 2a", "Unemployment benefits 2b","Unemployment benefits 2c"), ]
plot <- ggplot(filtered_data, aes(x = Experiment, y = ATE, fill = Time)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = ATE - 1.96*SE, ymax = ATE + 1.96*SE), position = position_dodge(width = 0.9), width = 0.2) +
  labs(x = "", y = "Weeks") +
  scale_fill_grey() +
  scale_fill_grey(start = 0.5, end = 0.9) +
  scale_y_continuous(limits = c(-2, 4), breaks = seq(-2, 3.5, by = 1)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 16, angle = 0, hjust = 0.5, vjust = 0.5),  # Adjust size here
    axis.text.y = element_text(size = 16),  # Adjust size here
    axis.title.x = element_text(size = 16),  # Adjust size here
    axis.title.y = element_text(size = 16),  # Adjust size here
    legend.text = element_text(size = 17),  # Adjust size here
    legend.title = element_blank(),  # Adjust size here
  )
# Save the plot as a PDF file
ggsave("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/Self_sufficient_ATE_appendix.pdf", plot, width = 10, height = 6, units = "in", dpi = 600)




################################################
########### Disability Pension (weeks) ################
################################################
#Laver lang table til at plotte Effekterne:
Disability_Pension <- rbind(c("Disability Pension","First 12 months","Social assistance",ATE_table$ATE...2[6],ATE_table$SE...3[6]))
Disability_Pension <- rbind(Disability_Pension, c("Disability Pension","12 to 60 months","Social assistance",ATE_table$ATE...5[6],ATE_table$SE...6[6]))
Disability_Pension <- rbind(Disability_Pension, c("Disability Pension","60 to 120 months","Social assistance",ATE_table$ATE...8[6],ATE_table$SE...9[6]))

Disability_Pension <- rbind(Disability_Pension, c("Disability Pension","First 12 months","Unemployment benefits 1",ATE_table$ATE...2[12],ATE_table$SE...3[12]))
Disability_Pension <- rbind(Disability_Pension, c("Disability Pension","12 to 60 months","Unemployment benefits 1",ATE_table$ATE...5[12],ATE_table$SE...6[12]))
Disability_Pension <- rbind(Disability_Pension, c("Disability Pension","60 to 120 months","Unemployment benefits 1",ATE_table$ATE...8[12],ATE_table$SE...9[12]))

Disability_Pension <- rbind(Disability_Pension, c("Disability Pension","First 12 months","Unemployment benefits 2a",ATE_table$ATE...2[18],ATE_table$SE...3[18]))
Disability_Pension <- rbind(Disability_Pension, c("Disability Pension","12 to 60 months","Unemployment benefits 2a",ATE_table$ATE...5[18],ATE_table$SE...6[18]))
Disability_Pension <- rbind(Disability_Pension, c("Disability Pension","60 to 120 months","Unemployment benefits 2a",ATE_table$ATE...8[18],ATE_table$SE...9[18]))

Disability_Pension <- rbind(Disability_Pension, c("Disability Pension","First 12 months","Unemployment benefits 2b",ATE_table$ATE...2[24],ATE_table$SE...3[24]))
Disability_Pension <- rbind(Disability_Pension, c("Disability Pension","12 to 60 months","Unemployment benefits 2b",ATE_table$ATE...5[24],ATE_table$SE...6[24]))
Disability_Pension <- rbind(Disability_Pension, c("Disability Pension","60 to 120 months","Unemployment benefits 2b",ATE_table$ATE...8[24],ATE_table$SE...9[24]))

Disability_Pension <- rbind(Disability_Pension, c("Disability Pension","First 12 months","Unemployment benefits 2c",ATE_table$ATE...2[30],ATE_table$SE...3[30]))
Disability_Pension <- rbind(Disability_Pension, c("Disability Pension","12 to 60 months","Unemployment benefits 2c",ATE_table$ATE...5[30],ATE_table$SE...6[30]))
Disability_Pension <- rbind(Disability_Pension, c("Disability Pension","60 to 120 months","Unemployment benefits 2c",ATE_table$ATE...8[30],ATE_table$SE...9[30]))


#laver til data.frame:
Disability_Pension = data.frame(Disability_Pension)
Disability_Pension$X4 <- as.numeric(Disability_Pension$X4)
Disability_Pension$X5 <- as.numeric(Disability_Pension$X5)

# Naming the columns using colnames()
colnames(Disability_Pension) <- c("Variable", "Time", "Experiment","ATE","SE")
Disability_Pension$Time <- factor(Disability_Pension$Time, levels = c("First 12 months", "12 to 60 months", "60 to 120 months"))

# Create the plot for Main text:
filtered_data <- Disability_Pension[Disability_Pension$Experiment %in% c("Social assistance", "Unemployment benefits 1"), ]
plot <- ggplot(filtered_data, aes(x = Experiment, y = ATE, fill = Time)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = ATE - 1.96*SE, ymax = ATE + 1.96*SE), position = position_dodge(width = 0.9), width = 0.2) +
  labs(x = "", y = "Weeks") +
  scale_fill_grey() +
  scale_fill_grey(start = 0.5, end = 0.9) +
  scale_y_continuous(limits = c(-1.5, 3), breaks = seq(-1, 3, by = 1)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5, vjust = 0.5),  # Adjust size here
    axis.text.y = element_text(size = 16),  # Adjust size here
    axis.title.x = element_text(size = 16),  # Adjust size here
    axis.title.y = element_text(size = 16),  # Adjust size here
    legend.text = element_text(size = 17),  # Adjust size here
    legend.title = element_blank(),  # Adjust size here
  )
# Save the plot as a PDF file
ggsave("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/Disability_Pension_ATE_main.pdf", plot, width = 8, height = 6, units = "in", dpi = 600)

# Create the plot for Appendix:
filtered_data <- Disability_Pension[Disability_Pension$Experiment %in% c("Unemployment benefits 2a", "Unemployment benefits 2b","Unemployment benefits 2c"), ]
plot <- ggplot(filtered_data, aes(x = Experiment, y = ATE, fill = Time)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = ATE - 1.96*SE, ymax = ATE + 1.96*SE), position = position_dodge(width = 0.9), width = 0.2) +
  labs(x = "", y = "Weeks") +
  scale_fill_grey() +
  scale_fill_grey(start = 0.5, end = 0.9) +
  scale_y_continuous(limits = c(-1.5, 3), breaks = seq(-1, 3, by = 1)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 16, angle = 0, hjust = 0.5, vjust = 0.5),  # Adjust size here
    axis.text.y = element_text(size = 16),  # Adjust size here
    axis.title.x = element_text(size = 16),  # Adjust size here
    axis.title.y = element_text(size = 16),  # Adjust size here
    legend.text = element_text(size = 17),  # Adjust size here
    legend.title = element_blank(),  # Adjust size here
  )
# Save the plot as a PDF file
ggsave("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/Disability_Pension_ATE_appendix.pdf", plot, width = 10, height = 6, units = "in", dpi = 600)
















################################################################################################################################################
################################################################################################################################################
################################################################################################################################################
################################################################################################################################################
                    ##################HTE plots
# Clear the environment
rm(list = ls())

# Set the significance level
alpha <- 0.05


################################################
########### Usage of N05 and N06 ################
################################################
new_loop=c("Social assistance","Unemployment benefits 1","Unemployment benefits 2a","Unemployment benefits 2b","Unemployment benefits 2c")
for(loop in 1:length(new_loop)) {

  raw_data <- read_excel(paste0("~/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/",new_loop[loop],"/GATE_table.xlsx"))
  
  DATA <- rbind(c(new_loop[loop],"Earnings","First 12 months","No",raw_data$estimate...2[6],raw_data$std.err...3[6]))
  DATA <- rbind(DATA, c(new_loop[loop],"Earnings","First 12 months","Yes",raw_data$estimate...2[7],raw_data$std.err...3[7]))
  DATA <- rbind(DATA, c(new_loop[loop],"Earnings","12 to 60 months","No",raw_data$estimate...5[6],raw_data$std.err...6[6]))
  DATA <- rbind(DATA, c(new_loop[loop],"Earnings","12 to 60 months","Yes",raw_data$estimate...5[7],raw_data$std.err...6[7]))
  DATA <- rbind(DATA, c(new_loop[loop],"Earnings","60 to 120 months","No",raw_data$estimate...8[6],raw_data$std.err...9[6]))
  DATA <- rbind(DATA, c(new_loop[loop],"Earnings","60 to 120 months","Yes",raw_data$estimate...8[7],raw_data$std.err...9[7]))
  
  DATA <- rbind(DATA, c(new_loop[loop],"Self-sufficient","First 12 months","No",raw_data$estimate...2[17],raw_data$std.err...3[17]))
  DATA <- rbind(DATA, c(new_loop[loop],"Self-sufficient","First 12 months","Yes",raw_data$estimate...2[18],raw_data$std.err...3[18]))
  DATA <- rbind(DATA, c(new_loop[loop],"Self-sufficient","12 to 60 months","No",raw_data$estimate...5[17],raw_data$std.err...6[17]))
  DATA <- rbind(DATA, c(new_loop[loop],"Self-sufficient","12 to 60 months","Yes",raw_data$estimate...5[18],raw_data$std.err...6[18]))
  DATA <- rbind(DATA, c(new_loop[loop],"Self-sufficient","60 to 120 months","No",raw_data$estimate...8[17],raw_data$std.err...9[17]))
  DATA <- rbind(DATA, c(new_loop[loop],"Self-sufficient","60 to 120 months","Yes",raw_data$estimate...8[18],raw_data$std.err...9[18]))
  
  DATA <- rbind(DATA, c(new_loop[loop],"N05","First 12 months","No",raw_data$estimate...2[28],raw_data$std.err...3[28]))
  DATA <- rbind(DATA, c(new_loop[loop],"N05","First 12 months","Yes",raw_data$estimate...2[29],raw_data$std.err...3[29]))
  DATA <- rbind(DATA, c(new_loop[loop],"N05","12 to 60 months","No",raw_data$estimate...5[28],raw_data$std.err...6[28]))
  DATA <- rbind(DATA, c(new_loop[loop],"N05","12 to 60 months","Yes",raw_data$estimate...5[29],raw_data$std.err...6[29]))
  DATA <- rbind(DATA, c(new_loop[loop],"N05","60 to 120 months","No",raw_data$estimate...8[28],raw_data$std.err...9[28]))
  DATA <- rbind(DATA, c(new_loop[loop],"N05","60 to 120 months","Yes",raw_data$estimate...8[29],raw_data$std.err...9[29]))
  
  DATA <- rbind(DATA, c(new_loop[loop],"N06","First 12 months","No",raw_data$estimate...2[39],raw_data$std.err...3[39]))
  DATA <- rbind(DATA, c(new_loop[loop],"N06","First 12 months","Yes",raw_data$estimate...2[40],raw_data$std.err...3[40]))
  DATA <- rbind(DATA, c(new_loop[loop],"N06","12 to 60 months","No",raw_data$estimate...5[39],raw_data$std.err...6[39]))
  DATA <- rbind(DATA, c(new_loop[loop],"N06","12 to 60 months","Yes",raw_data$estimate...5[40],raw_data$std.err...6[40]))
  DATA <- rbind(DATA, c(new_loop[loop],"N06","60 to 120 months","No",raw_data$estimate...8[39],raw_data$std.err...9[39]))
  DATA <- rbind(DATA, c(new_loop[loop],"N06","60 to 120 months","Yes",raw_data$estimate...8[40],raw_data$std.err...9[40]))
  
  assign(paste0("DATA_", new_loop[loop]), DATA)  
}

#Append:
DATA <- rbind(`DATA_Social assistance`, `DATA_Unemployment benefits 1`, `DATA_Unemployment benefits 2a`, `DATA_Unemployment benefits 2b`, `DATA_Unemployment benefits 2c`)

#laver til data.frame:
DATA = data.frame(DATA)
DATA$X5 <- as.numeric(DATA$X5)
DATA$X6 <- as.numeric(DATA$X6)
# Naming the columns using colnames()
colnames(DATA) <- c("Experiment","Variable", "Time","Use","ATE","SE")
DATA$Time <- factor(DATA$Time, levels = c("First 12 months", "12 to 60 months", "60 to 120 months"))
DATA$Use <- factor(DATA$Use, levels = c("No","Yes"))


data <- DATA 
## Her teste om der er significant forskel mellem Yes and No:
# Subset the data for 'Yes' and 'No' uses
data_yes <- subset(data, Use == "Yes")
data_no <- subset(data, Use == "No")

# Extract the coefficients and standard errors as vectors
coef_yes <- data_yes$ATE
se_yes <- data_yes$SE
coef_no <- data_no$ATE
se_no <- data_no$SE

# Calculate the test statistic
t_stat <- (coef_yes - coef_no) / sqrt((se_yes^2 + se_no^2))

# Compare the test statistic to the critical value
p_value <- 2 * pt(-abs(t_stat), 1000)
p_value <- rep(c(p_value, NA), each = 2)
p_value <- p_value[1:(length(p_value) - 2)]
# Set every second value to NA using a loop,such that only for YES we see the test result:
for (i in seq(1, length(p_value), by = 2)) {
  p_value[i] <- NA
}
#Add p-value to data:
data = cbind(data, p_value)

# Define the significance levels and corresponding labels
significance_labels <- c("(***)" = 0.01, "(**)" = 0.05, "(*)" = 0.10)

# Create a new column for significance labels based on p-value
data$Significance <- factor(
         ifelse(data$p_value < 0.05, "(**)",
                ifelse(data$p_value < 0.10, "(*)", "")),
  levels = names(significance_labels)
)

#Legend label:
leg <- "Usage of N05 or N06 in the year prior to the trial:"


##Laver loop over experiments:
experiments <- unique(data$Experiment)

for (experiment in experiments) {

# Plotter N05:
data_new = subset(data, Variable == "N05" & Experiment==experiment)
plot_N05 <- ggplot(data_new, aes(x = Time, y = ATE, fill = Use, group = Use)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = ATE - 1.96 * SE, ymax = ATE + 1.96 * SE), position = position_dodge(width = 0.9), width = 0.25) +
  labs(title = "", x = "", y = "Defined Daily Doses (DDD)") +
  scale_fill_grey(start = 0.5, end = 0.85) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5, vjust = 0.5),  # Adjust size here
    axis.text.y = element_text(size = 16),  # Adjust size here
    axis.title.x = element_text(size = 16),  # Adjust size here
    axis.title.y = element_text(size = 16),  # Adjust size here
    legend.text = element_text(size = 17),  # Adjust size here
    legend.title = element_text(size = 18)
  ) +
  guides(fill = guide_legend(title = leg)) +
  # Add significance indicators using geom_text
  geom_text(
    aes(label = Significance, y = max(abs(data_new$ATE) + 2 * data_new$SE)),
    position = position_dodge(width = 0),
    size = 6,  # Increased text size for significance indicators
    vjust = 0, 
    hjust = 0.5
  )
ggsave(paste("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/", experiment, "/N05_HTE_used_NO5orN06.pdf", sep = ""), plot_N05, device = "pdf", width = 12, height = 8)


# Plotter N06:
data_new = subset(data, Variable == "N06" & Experiment==experiment)
plot_N06 <- ggplot(data_new, aes(x = Time, y = ATE, fill = Use, group = Use)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = ATE - 1.96 * SE, ymax = ATE + 1.96 * SE), position = position_dodge(width = 0.9), width = 0.25) +
  labs(title = "", x = "", y = "Defined Daily Doses (DDD)") +
  scale_fill_grey(start = 0.5, end = 0.85) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5, vjust = 0.5),  # Adjust size here
    axis.text.y = element_text(size = 16),  # Adjust size here
    axis.title.x = element_text(size = 16),  # Adjust size here
    axis.title.y = element_text(size = 16),  # Adjust size here
    legend.text = element_text(size = 17),  # Adjust size here
    legend.title = element_text(size = 18)
  ) +
  guides(fill = guide_legend(title = leg)) +
  # Add significance indicators using geom_text
  geom_text(
    aes(label = Significance, y =max(abs(data_new$ATE) + 2 * data_new$SE)),
    position = position_dodge(width = 0),
    size = 6, vjust = 0, hjust=0.5
  ) 
ggsave(paste("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/", experiment, "/N06_HTE_used_NO5orN06.pdf", sep = ""), plot_N06, device = "pdf", width = 12, height = 8)

# Plotter Earnings:
data_new = subset(data, Variable == "Earnings" & Experiment==experiment)
plot_Earnings <- ggplot(data_new, aes(x = Time, y = ATE, fill = Use, group = Use)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = ATE - 1.96 * SE, ymax = ATE + 1.96 * SE), position = position_dodge(width = 0.9), width = 0.25) +
  labs(title = "", x = "", y = "US Dollars") +
  scale_fill_grey(start = 0.5, end = 0.85) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5, vjust = 0.5),  # Adjust size here
    axis.text.y = element_text(size = 16),  # Adjust size here
    axis.title.x = element_text(size = 16),  # Adjust size here
    axis.title.y = element_text(size = 16),  # Adjust size here
    legend.text = element_text(size = 17),  # Adjust size here
    legend.title = element_text(size = 18)
  ) +
  guides(fill = guide_legend(title = leg)) +
  # Add significance indicators using geom_text
  geom_text(
    aes(label = Significance, y = max(abs(data_new$ATE) + 2 * data_new$SE)),
    position = position_dodge(width = 0),
    size = 6, vjust = 0, hjust=0.5
  ) 
ggsave(paste("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/", experiment, "/Earnings_HTE_used_NO5orN06.pdf", sep = ""), plot_Earnings, device = "pdf", width = 12, height = 8)


# Plotter Self-sufficient:
data_new = subset(data, Variable == "Self-sufficient" & Experiment==experiment)
plot_Self <- ggplot(data_new, aes(x = Time, y = ATE, fill = Use, group = Use)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = ATE - 1.96 * SE, ymax = ATE + 1.96 * SE), position = position_dodge(width = 0.9), width = 0.25) +
  labs(title = "", x = "", y = "Weeks") +
  scale_fill_grey(start = 0.5, end = 0.85) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5, vjust = 0.5),  # Adjust size here
    axis.text.y = element_text(size = 16),  # Adjust size here
    axis.title.x = element_text(size = 16),  # Adjust size here
    axis.title.y = element_text(size = 16),  # Adjust size here
    legend.text = element_text(size = 17),  # Adjust size here
    legend.title = element_text(size = 18)
  ) +
  guides(fill = guide_legend(title = leg)) +
  # Add significance indicators using geom_text
  geom_text(
    aes(label = Significance, y = max(abs(data_new$ATE) + 2 * data_new$SE)),
    position = position_dodge(width = 0),
    size = 6, vjust = 0, hjust=0.5
  ) 
ggsave(paste("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/", experiment, "/Self_sufficient_HTE_used_NO5orN06.pdf", sep = ""), plot_Self, device = "pdf", width = 12, height = 8)

}




################################################
########### Visits to psychiatry ################
################################################
new_loop=c("Social assistance","Unemployment benefits 1","Unemployment benefits 2a","Unemployment benefits 2b","Unemployment benefits 2c")
for(loop in 1:length(new_loop)) {
  
  raw_data <- read_excel(paste0("~/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/",new_loop[loop],"/GATE_table.xlsx"))
  
  DATA <- rbind(c(new_loop[loop],"Earnings","First 12 months","No",raw_data$estimate...2[8],raw_data$std.err...3[8]))
  DATA <- rbind(DATA, c(new_loop[loop],"Earnings","First 12 months","Yes",raw_data$estimate...2[9],raw_data$std.err...3[9]))
  DATA <- rbind(DATA, c(new_loop[loop],"Earnings","12 to 60 months","No",raw_data$estimate...5[8],raw_data$std.err...6[8]))
  DATA <- rbind(DATA, c(new_loop[loop],"Earnings","12 to 60 months","Yes",raw_data$estimate...5[9],raw_data$std.err...6[9]))
  DATA <- rbind(DATA, c(new_loop[loop],"Earnings","60 to 120 months","No",raw_data$estimate...8[8],raw_data$std.err...9[8]))
  DATA <- rbind(DATA, c(new_loop[loop],"Earnings","60 to 120 months","Yes",raw_data$estimate...8[9],raw_data$std.err...9[9]))
  
  DATA <- rbind(DATA, c(new_loop[loop],"Self-sufficient","First 12 months","No",raw_data$estimate...2[19],raw_data$std.err...3[19]))
  DATA <- rbind(DATA, c(new_loop[loop],"Self-sufficient","First 12 months","Yes",raw_data$estimate...2[20],raw_data$std.err...3[20]))
  DATA <- rbind(DATA, c(new_loop[loop],"Self-sufficient","12 to 60 months","No",raw_data$estimate...5[19],raw_data$std.err...6[19]))
  DATA <- rbind(DATA, c(new_loop[loop],"Self-sufficient","12 to 60 months","Yes",raw_data$estimate...5[20],raw_data$std.err...6[20]))
  DATA <- rbind(DATA, c(new_loop[loop],"Self-sufficient","60 to 120 months","No",raw_data$estimate...8[19],raw_data$std.err...9[19]))
  DATA <- rbind(DATA, c(new_loop[loop],"Self-sufficient","60 to 120 months","Yes",raw_data$estimate...8[20],raw_data$std.err...9[20]))
  
  DATA <- rbind(DATA, c(new_loop[loop],"N05","First 12 months","No",raw_data$estimate...2[30],raw_data$std.err...3[30]))
  DATA <- rbind(DATA, c(new_loop[loop],"N05","First 12 months","Yes",raw_data$estimate...2[31],raw_data$std.err...3[31]))
  DATA <- rbind(DATA, c(new_loop[loop],"N05","12 to 60 months","No",raw_data$estimate...5[30],raw_data$std.err...6[30]))
  DATA <- rbind(DATA, c(new_loop[loop],"N05","12 to 60 months","Yes",raw_data$estimate...5[31],raw_data$std.err...6[31]))
  DATA <- rbind(DATA, c(new_loop[loop],"N05","60 to 120 months","No",raw_data$estimate...8[30],raw_data$std.err...9[30]))
  DATA <- rbind(DATA, c(new_loop[loop],"N05","60 to 120 months","Yes",raw_data$estimate...8[31],raw_data$std.err...9[31]))
  
  DATA <- rbind(DATA, c(new_loop[loop],"N06","First 12 months","No",raw_data$estimate...2[41],raw_data$std.err...3[41]))
  DATA <- rbind(DATA, c(new_loop[loop],"N06","First 12 months","Yes",raw_data$estimate...2[42],raw_data$std.err...3[42]))
  DATA <- rbind(DATA, c(new_loop[loop],"N06","12 to 60 months","No",raw_data$estimate...5[41],raw_data$std.err...6[41]))
  DATA <- rbind(DATA, c(new_loop[loop],"N06","12 to 60 months","Yes",raw_data$estimate...5[42],raw_data$std.err...6[42]))
  DATA <- rbind(DATA, c(new_loop[loop],"N06","60 to 120 months","No",raw_data$estimate...8[41],raw_data$std.err...9[41]))
  DATA <- rbind(DATA, c(new_loop[loop],"N06","60 to 120 months","Yes",raw_data$estimate...8[42],raw_data$std.err...9[42]))
  
  assign(paste0("DATA_", new_loop[loop]), DATA)  
}

#Append:
DATA <- rbind(`DATA_Social assistance`, `DATA_Unemployment benefits 1`, `DATA_Unemployment benefits 2a`, `DATA_Unemployment benefits 2b`, `DATA_Unemployment benefits 2c`)

#laver til data.frame:
DATA = data.frame(DATA)
DATA$X5 <- as.numeric(DATA$X5)
DATA$X6 <- as.numeric(DATA$X6)
# Naming the columns using colnames()
colnames(DATA) <- c("Experiment","Variable", "Time","Use","ATE","SE")
DATA$Time <- factor(DATA$Time, levels = c("First 12 months", "12 to 60 months", "60 to 120 months"))
DATA$Use <- factor(DATA$Use, levels = c("No","Yes"))


data <- DATA 
## Her teste om der er significant forskel mellem Yes and No:
# Subset the data for 'Yes' and 'No' uses
data_yes <- subset(data, Use == "Yes")
data_no <- subset(data, Use == "No")

# Extract the coefficients and standard errors as vectors
coef_yes <- data_yes$ATE
se_yes <- data_yes$SE
coef_no <- data_no$ATE
se_no <- data_no$SE

# Calculate the test statistic
t_stat <- (coef_yes - coef_no) / sqrt((se_yes^2 + se_no^2))

# Compare the test statistic to the critical value
p_value <- 2 * pt(-abs(t_stat), 1000)
p_value <- rep(c(p_value, NA), each = 2)
p_value <- p_value[1:(length(p_value) - 2)]
# Set every second value to NA using a loop,such that only for YES we see the test result:
for (i in seq(1, length(p_value), by = 2)) {
  p_value[i] <- NA
}
#Add p-value to data:
data = cbind(data, p_value)

# Define the significance levels and corresponding labels
significance_labels <- c("(***)" = 0.01, "(**)" = 0.05, "(*)" = 0.10)

# Create a new column for significance labels based on p-value
data$Significance <- factor(
         ifelse(data$p_value < 0.05, "(**)",
                ifelse(data$p_value < 0.10, "(*)", "")),
  levels = names(significance_labels)
)

#Legend label:
leg <- "Any visits to psychiatry five years prior to the trial:"


##Laver loop over experiments:
experiments <- unique(data$Experiment)

for (experiment in experiments) {
  
  # Plotter N05:
  data_new = subset(data, Variable == "N05" & Experiment==experiment)
  plot_N05 <- ggplot(data_new, aes(x = Time, y = ATE, fill = Use, group = Use)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin = ATE - 1.96 * SE, ymax = ATE + 1.96 * SE), position = position_dodge(width = 0.9), width = 0.25) +
    labs(title = "", x = "", y = "Defined Daily Doses (DDD)") +
    scale_fill_grey(start = 0.5, end = 0.85) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5, vjust = 0.5),  # Adjust size here
      axis.text.y = element_text(size = 16),  # Adjust size here
      axis.title.x = element_text(size = 16),  # Adjust size here
      axis.title.y = element_text(size = 16),  # Adjust size here
      legend.text = element_text(size = 17),  # Adjust size here
      legend.title = element_text(size = 18)
    ) +
    guides(fill = guide_legend(title = leg)) +
    # Add significance indicators using geom_text
    geom_text(
      aes(label = Significance, y = max(abs(data_new$ATE) + 2 * data_new$SE)),
      position = position_dodge(width = 0),
      size = 6, vjust = 0, hjust=0.5
    ) 
  ggsave(paste("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/", experiment, "/N05_HTE_Visits_to_psychiatry.pdf", sep = ""), plot_N05, device = "pdf", width = 12, height = 8)
  
  
  # Plotter N06:
  data_new = subset(data, Variable == "N06" & Experiment==experiment)
  plot_N06 <- ggplot(data_new, aes(x = Time, y = ATE, fill = Use, group = Use)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin = ATE - 1.96 * SE, ymax = ATE + 1.96 * SE), position = position_dodge(width = 0.9), width = 0.25) +
    labs(title = "", x = "", y = "Defined Daily Doses (DDD)") +
    scale_fill_grey(start = 0.5, end = 0.85) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5, vjust = 0.5),  # Adjust size here
      axis.text.y = element_text(size = 16),  # Adjust size here
      axis.title.x = element_text(size = 16),  # Adjust size here
      axis.title.y = element_text(size = 16),  # Adjust size here
      legend.text = element_text(size = 17),  # Adjust size here
      legend.title = element_text(size = 18)
    ) +
    guides(fill = guide_legend(title = leg)) +
    # Add significance indicators using geom_text
    geom_text(
      aes(label = Significance, y =max(abs(data_new$ATE) + 2 * data_new$SE)),
      position = position_dodge(width = 0),
      size = 6, vjust = 0, hjust=0.5
    ) 
  ggsave(paste("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/", experiment, "/N06_HTE_Visits_to_psychiatry.pdf", sep = ""), plot_N06, device = "pdf", width = 12, height = 8)
  
  # Plotter Earnings:
  data_new = subset(data, Variable == "Earnings" & Experiment==experiment)
  plot_Earnings <- ggplot(data_new, aes(x = Time, y = ATE, fill = Use, group = Use)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin = ATE - 1.96 * SE, ymax = ATE + 1.96 * SE), position = position_dodge(width = 0.9), width = 0.25) +
    labs(title = "", x = "", y = "US Dollars") +
    scale_fill_grey(start = 0.5, end = 0.85) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5, vjust = 0.5),  # Adjust size here
      axis.text.y = element_text(size = 16),  # Adjust size here
      axis.title.x = element_text(size = 16),  # Adjust size here
      axis.title.y = element_text(size = 16),  # Adjust size here
      legend.text = element_text(size = 17),  # Adjust size here
      legend.title = element_text(size = 18)
    ) +
    guides(fill = guide_legend(title = leg)) +
    # Add significance indicators using geom_text
    geom_text(
      aes(label = Significance, y = max(abs(data_new$ATE) + 2 * data_new$SE)),
      position = position_dodge(width = 0),
      size = 6, vjust = 0, hjust=0.5
    ) 
  ggsave(paste("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/", experiment, "/Earnings_HTE_Visits_to_psychiatry.pdf", sep = ""), plot_Earnings, device = "pdf", width = 12, height = 8)
  
  
  # Plotter Self-sufficient:
  data_new = subset(data, Variable == "Self-sufficient" & Experiment==experiment)
  plot_Self <- ggplot(data_new, aes(x = Time, y = ATE, fill = Use, group = Use)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin = ATE - 1.96 * SE, ymax = ATE + 1.96 * SE), position = position_dodge(width = 0.9), width = 0.25) +
    labs(title = "", x = "", y = "Weeks") +
    scale_fill_grey(start = 0.5, end = 0.85) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5, vjust = 0.5),  # Adjust size here
      axis.text.y = element_text(size = 16),  # Adjust size here
      axis.title.x = element_text(size = 16),  # Adjust size here
      axis.title.y = element_text(size = 16),  # Adjust size here
      legend.text = element_text(size = 17),  # Adjust size here
      legend.title = element_text(size = 18)
    ) +
    guides(fill = guide_legend(title = leg)) +
    # Add significance indicators using geom_text
    geom_text(
      aes(label = Significance, y = max(abs(data_new$ATE) + 2 * data_new$SE)),
      position = position_dodge(width = 0),
      size = 6, vjust = 0, hjust=0.5
    ) 
  ggsave(paste("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/", experiment, "/Self_sufficient_HTE_Visits_to_psychiatry.pdf", sep = ""), plot_Self, device = "pdf", width = 12, height = 8)
}






################################################
########### Psychiatry diagnosis ################
################################################
for(loop in 1:length(new_loop)) {
  
  raw_data <- read_excel(paste0("~/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/",new_loop[loop],"/GATE_table.xlsx"))
  
  DATA <- rbind(c(new_loop[loop],"Earnings","First 12 months","No",raw_data$estimate...2[10],raw_data$std.err...3[10]))
  DATA <- rbind(DATA, c(new_loop[loop],"Earnings","First 12 months","Yes",raw_data$estimate...2[11],raw_data$std.err...3[11]))
  DATA <- rbind(DATA, c(new_loop[loop],"Earnings","12 to 60 months","No",raw_data$estimate...5[10],raw_data$std.err...6[10]))
  DATA <- rbind(DATA, c(new_loop[loop],"Earnings","12 to 60 months","Yes",raw_data$estimate...5[11],raw_data$std.err...6[11]))
  DATA <- rbind(DATA, c(new_loop[loop],"Earnings","60 to 120 months","No",raw_data$estimate...8[10],raw_data$std.err...9[10]))
  DATA <- rbind(DATA, c(new_loop[loop],"Earnings","60 to 120 months","Yes",raw_data$estimate...8[11],raw_data$std.err...9[11]))
  
  DATA <- rbind(DATA, c(new_loop[loop],"Self-sufficient","First 12 months","No",raw_data$estimate...2[21],raw_data$std.err...3[21]))
  DATA <- rbind(DATA, c(new_loop[loop],"Self-sufficient","First 12 months","Yes",raw_data$estimate...2[22],raw_data$std.err...3[22]))
  DATA <- rbind(DATA, c(new_loop[loop],"Self-sufficient","12 to 60 months","No",raw_data$estimate...5[21],raw_data$std.err...6[21]))
  DATA <- rbind(DATA, c(new_loop[loop],"Self-sufficient","12 to 60 months","Yes",raw_data$estimate...5[22],raw_data$std.err...6[22]))
  DATA <- rbind(DATA, c(new_loop[loop],"Self-sufficient","60 to 120 months","No",raw_data$estimate...8[21],raw_data$std.err...9[21]))
  DATA <- rbind(DATA, c(new_loop[loop],"Self-sufficient","60 to 120 months","Yes",raw_data$estimate...8[22],raw_data$std.err...9[22]))
  
  DATA <- rbind(DATA, c(new_loop[loop],"N05","First 12 months","No",raw_data$estimate...2[32],raw_data$std.err...3[32]))
  DATA <- rbind(DATA, c(new_loop[loop],"N05","First 12 months","Yes",raw_data$estimate...2[33],raw_data$std.err...3[33]))
  DATA <- rbind(DATA, c(new_loop[loop],"N05","12 to 60 months","No",raw_data$estimate...5[32],raw_data$std.err...6[32]))
  DATA <- rbind(DATA, c(new_loop[loop],"N05","12 to 60 months","Yes",raw_data$estimate...5[33],raw_data$std.err...6[33]))
  DATA <- rbind(DATA, c(new_loop[loop],"N05","60 to 120 months","No",raw_data$estimate...8[32],raw_data$std.err...9[32]))
  DATA <- rbind(DATA, c(new_loop[loop],"N05","60 to 120 months","Yes",raw_data$estimate...8[33],raw_data$std.err...9[33]))
  
  DATA <- rbind(DATA, c(new_loop[loop],"N06","First 12 months","No",raw_data$estimate...2[43],raw_data$std.err...3[43]))
  DATA <- rbind(DATA, c(new_loop[loop],"N06","First 12 months","Yes",raw_data$estimate...2[44],raw_data$std.err...3[44]))
  DATA <- rbind(DATA, c(new_loop[loop],"N06","12 to 60 months","No",raw_data$estimate...5[43],raw_data$std.err...6[43]))
  DATA <- rbind(DATA, c(new_loop[loop],"N06","12 to 60 months","Yes",raw_data$estimate...5[44],raw_data$std.err...6[44]))
  DATA <- rbind(DATA, c(new_loop[loop],"N06","60 to 120 months","No",raw_data$estimate...8[43],raw_data$std.err...9[43]))
  DATA <- rbind(DATA, c(new_loop[loop],"N06","60 to 120 months","Yes",raw_data$estimate...8[44],raw_data$std.err...9[44]))
  
  assign(paste0("DATA_", new_loop[loop]), DATA)  
}

#Append:
DATA <- rbind(`DATA_Social assistance`, `DATA_Unemployment benefits 1`, `DATA_Unemployment benefits 2a`, `DATA_Unemployment benefits 2b`, `DATA_Unemployment benefits 2c`)


#laver til data.frame:
DATA = data.frame(DATA)
DATA$X5 <- as.numeric(DATA$X5)
DATA$X6 <- as.numeric(DATA$X6)
# Naming the columns using colnames()
colnames(DATA) <- c("Experiment","Variable", "Time","Use","ATE","SE")
DATA$Time <- factor(DATA$Time, levels = c("First 12 months", "12 to 60 months", "60 to 120 months"))
DATA$Use <- factor(DATA$Use, levels = c("No","Yes"))

data <- DATA 
## Her teste om der er significant forskel mellem Yes and No:
# Subset the data for 'Yes' and 'No' uses
data_yes <- subset(data, Use == "Yes")
data_no <- subset(data, Use == "No")

# Extract the coefficients and standard errors as vectors
coef_yes <- data_yes$ATE
se_yes <- data_yes$SE
coef_no <- data_no$ATE
se_no <- data_no$SE

# Calculate the test statistic
t_stat <- (coef_yes - coef_no) / sqrt((se_yes^2 + se_no^2))

# Compare the test statistic to the critical value
p_value <- 2 * pt(-abs(t_stat), 1000)
p_value <- rep(c(p_value, NA), each = 2)
p_value <- p_value[1:(length(p_value) - 2)]
# Set every second value to NA using a loop,such that only for YES we see the test result:
for (i in seq(1, length(p_value), by = 2)) {
  p_value[i] <- NA
}
#Add p-value to data:
data = cbind(data, p_value)

# Define the significance levels and corresponding labels
significance_labels <- c("(***)" = 0.01, "(**)" = 0.05, "(*)" = 0.10)

# Create a new column for significance labels based on p-value
data$Significance <- factor(
         ifelse(data$p_value < 0.05, "(**)",
                ifelse(data$p_value < 0.10, "(*)", "")),
  levels = names(significance_labels)
)

#Legend label:
leg <- "Any psychiatry diagnosis five years prior to the trial:"


##Laver loop over experiments:
experiments <- unique(data$Experiment)

for (experiment in experiments) {
  
  # Plotter N05:
  data_new = subset(data, Variable == "N05" & Experiment==experiment)
  plot_N05 <- ggplot(data_new, aes(x = Time, y = ATE, fill = Use, group = Use)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin = ATE - 1.96 * SE, ymax = ATE + 1.96 * SE), position = position_dodge(width = 0.9), width = 0.25) +
    labs(title = "", x = "", y = "Defined Daily Doses (DDD)") +
    scale_fill_grey(start = 0.5, end = 0.85) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5, vjust = 0.5),  # Adjust size here
      axis.text.y = element_text(size = 16),  # Adjust size here
      axis.title.x = element_text(size = 16),  # Adjust size here
      axis.title.y = element_text(size = 16),  # Adjust size here
      legend.text = element_text(size = 17),  # Adjust size here
      legend.title = element_text(size = 18)
    ) +
    guides(fill = guide_legend(title = leg)) +
    # Add significance indicators using geom_text
    geom_text(
      aes(label = Significance, y = max(abs(data_new$ATE) + 2 * data_new$SE)),
      position = position_dodge(width = 0),
      size = 6, vjust = 0, hjust=0.5
    ) 
  ggsave(paste("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/", experiment, "/N05_HTE_Psychiatry_diagnosis.pdf", sep = ""), plot_N05, device = "pdf", width = 12, height = 8)
  
  
  # Plotter N06:
  data_new = subset(data, Variable == "N06" & Experiment==experiment)
  plot_N06 <- ggplot(data_new, aes(x = Time, y = ATE, fill = Use, group = Use)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin = ATE - 1.96 * SE, ymax = ATE + 1.96 * SE), position = position_dodge(width = 0.9), width = 0.25) +
    labs(title = "", x = "", y = "Defined Daily Doses (DDD)") +
    scale_fill_grey(start = 0.5, end = 0.85) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5, vjust = 0.5),  # Adjust size here
      axis.text.y = element_text(size = 16),  # Adjust size here
      axis.title.x = element_text(size = 16),  # Adjust size here
      axis.title.y = element_text(size = 16),  # Adjust size here
      legend.text = element_text(size = 17),  # Adjust size here
      legend.title = element_text(size = 18)
    ) +
    guides(fill = guide_legend(title = leg)) +
    # Add significance indicators using geom_text
    geom_text(
      aes(label = Significance, y =max(abs(data_new$ATE) + 2 * data_new$SE)),
      position = position_dodge(width = 0),
      size = 6, vjust = 0, hjust=0.5
    ) 
  ggsave(paste("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/", experiment, "/N06_HTE_Psychiatry_diagnosis.pdf", sep = ""), plot_N06, device = "pdf", width = 12, height = 8)
  
  # Plotter Earnings:
  data_new = subset(data, Variable == "Earnings" & Experiment==experiment)
  plot_Earnings <- ggplot(data_new, aes(x = Time, y = ATE, fill = Use, group = Use)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin = ATE - 1.96 * SE, ymax = ATE + 1.96 * SE), position = position_dodge(width = 0.9), width = 0.25) +
    labs(title = "", x = "", y = "US Dollars") +
    scale_fill_grey(start = 0.5, end = 0.85) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5, vjust = 0.5),  # Adjust size here
      axis.text.y = element_text(size = 16),  # Adjust size here
      axis.title.x = element_text(size = 16),  # Adjust size here
      axis.title.y = element_text(size = 16),  # Adjust size here
      legend.text = element_text(size = 17),  # Adjust size here
      legend.title = element_text(size = 18)
    ) +
    guides(fill = guide_legend(title = leg)) +
    # Add significance indicators using geom_text
    geom_text(
      aes(label = Significance, y = max(abs(data_new$ATE) + 2 * data_new$SE)),
      position = position_dodge(width = 0),
      size = 6, vjust = 0, hjust=0.5
    ) 
  ggsave(paste("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/", experiment, "/Earnings_HTE_Psychiatry_diagnosis.pdf", sep = ""), plot_Earnings, device = "pdf", width = 12, height = 8)
  
  
  # Plotter Self-sufficient:
  data_new = subset(data, Variable == "Self-sufficient" & Experiment==experiment)
  plot_Self <- ggplot(data_new, aes(x = Time, y = ATE, fill = Use, group = Use)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin = ATE - 1.96 * SE, ymax = ATE + 1.96 * SE), position = position_dodge(width = 0.9), width = 0.25) +
    labs(title = "", x = "", y = "Weeks") +
    scale_fill_grey(start = 0.5, end = 0.85) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5, vjust = 0.5),  # Adjust size here
      axis.text.y = element_text(size = 16),  # Adjust size here
      axis.title.x = element_text(size = 16),  # Adjust size here
      axis.title.y = element_text(size = 16),  # Adjust size here
      legend.text = element_text(size = 17),  # Adjust size here
      legend.title = element_text(size = 18)
    ) +
    guides(fill = guide_legend(title = leg)) +
    # Add significance indicators using geom_text
    geom_text(
      aes(label = Significance, y = max(abs(data_new$ATE) + 2 * data_new$SE)),
      position = position_dodge(width = 0),
      size = 6, vjust = 0, hjust=0.5
    ) 
  ggsave(paste("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/", experiment, "/Self_sufficient_HTE_Psychiatry_diagnosis.pdf", sep = ""), plot_Self, device = "pdf", width = 12, height = 8)
}



################################################
########### GP services >15 ################
################################################
for(loop in 1:length(new_loop)) {
  
  raw_data <- read_excel(paste0("~/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/",new_loop[loop],"/GATE_table.xlsx"))
  
  DATA <- rbind(c(new_loop[loop],"Earnings","First 12 months","No",raw_data$estimate...2[12],raw_data$std.err...3[12]))
  DATA <- rbind(DATA, c(new_loop[loop],"Earnings","First 12 months","Yes",raw_data$estimate...2[13],raw_data$std.err...3[13]))
  DATA <- rbind(DATA, c(new_loop[loop],"Earnings","12 to 60 months","No",raw_data$estimate...5[12],raw_data$std.err...6[12]))
  DATA <- rbind(DATA, c(new_loop[loop],"Earnings","12 to 60 months","Yes",raw_data$estimate...5[13],raw_data$std.err...6[13]))
  DATA <- rbind(DATA, c(new_loop[loop],"Earnings","60 to 120 months","No",raw_data$estimate...8[12],raw_data$std.err...9[12]))
  DATA <- rbind(DATA, c(new_loop[loop],"Earnings","60 to 120 months","Yes",raw_data$estimate...8[13],raw_data$std.err...9[13]))
  
  DATA <- rbind(DATA, c(new_loop[loop],"Self-sufficient","First 12 months","No",raw_data$estimate...2[23],raw_data$std.err...3[23]))
  DATA <- rbind(DATA, c(new_loop[loop],"Self-sufficient","First 12 months","Yes",raw_data$estimate...2[24],raw_data$std.err...3[24]))
  DATA <- rbind(DATA, c(new_loop[loop],"Self-sufficient","12 to 60 months","No",raw_data$estimate...5[23],raw_data$std.err...6[23]))
  DATA <- rbind(DATA, c(new_loop[loop],"Self-sufficient","12 to 60 months","Yes",raw_data$estimate...5[24],raw_data$std.err...6[24]))
  DATA <- rbind(DATA, c(new_loop[loop],"Self-sufficient","60 to 120 months","No",raw_data$estimate...8[23],raw_data$std.err...9[23]))
  DATA <- rbind(DATA, c(new_loop[loop],"Self-sufficient","60 to 120 months","Yes",raw_data$estimate...8[24],raw_data$std.err...9[24]))
  
  DATA <- rbind(DATA, c(new_loop[loop],"N05","First 12 months","No",raw_data$estimate...2[34],raw_data$std.err...3[34]))
  DATA <- rbind(DATA, c(new_loop[loop],"N05","First 12 months","Yes",raw_data$estimate...2[35],raw_data$std.err...3[35]))
  DATA <- rbind(DATA, c(new_loop[loop],"N05","12 to 60 months","No",raw_data$estimate...5[34],raw_data$std.err...6[34]))
  DATA <- rbind(DATA, c(new_loop[loop],"N05","12 to 60 months","Yes",raw_data$estimate...5[35],raw_data$std.err...6[35]))
  DATA <- rbind(DATA, c(new_loop[loop],"N05","60 to 120 months","No",raw_data$estimate...8[34],raw_data$std.err...9[34]))
  DATA <- rbind(DATA, c(new_loop[loop],"N05","60 to 120 months","Yes",raw_data$estimate...8[35],raw_data$std.err...9[35]))
  
  DATA <- rbind(DATA, c(new_loop[loop],"N06","First 12 months","No",raw_data$estimate...2[45],raw_data$std.err...3[45]))
  DATA <- rbind(DATA, c(new_loop[loop],"N06","First 12 months","Yes",raw_data$estimate...2[46],raw_data$std.err...3[46]))
  DATA <- rbind(DATA, c(new_loop[loop],"N06","12 to 60 months","No",raw_data$estimate...5[45],raw_data$std.err...6[45]))
  DATA <- rbind(DATA, c(new_loop[loop],"N06","12 to 60 months","Yes",raw_data$estimate...5[46],raw_data$std.err...6[46]))
  DATA <- rbind(DATA, c(new_loop[loop],"N06","60 to 120 months","No",raw_data$estimate...8[45],raw_data$std.err...9[45]))
  DATA <- rbind(DATA, c(new_loop[loop],"N06","60 to 120 months","Yes",raw_data$estimate...8[46],raw_data$std.err...9[46]))
  
  assign(paste0("DATA_", new_loop[loop]), DATA)  
}

#Append:
DATA <- rbind(`DATA_Social assistance`, `DATA_Unemployment benefits 1`, `DATA_Unemployment benefits 2a`, `DATA_Unemployment benefits 2b`, `DATA_Unemployment benefits 2c`)


#laver til data.frame:
DATA = data.frame(DATA)
DATA$X5 <- as.numeric(DATA$X5)
DATA$X6 <- as.numeric(DATA$X6)
# Naming the columns using colnames()
colnames(DATA) <- c("Experiment","Variable", "Time","Use","ATE","SE")
DATA$Time <- factor(DATA$Time, levels = c("First 12 months", "12 to 60 months", "60 to 120 months"))
DATA$Use <- factor(DATA$Use, levels = c("No","Yes"))

data <- DATA 
## Her teste om der er significant forskel mellem Yes and No:
# Subset the data for 'Yes' and 'No' uses
data_yes <- subset(data, Use == "Yes")
data_no <- subset(data, Use == "No")

# Extract the coefficients and standard errors as vectors
coef_yes <- data_yes$ATE
se_yes <- data_yes$SE
coef_no <- data_no$ATE
se_no <- data_no$SE

# Calculate the test statistic
t_stat <- (coef_yes - coef_no) / sqrt((se_yes^2 + se_no^2))

# Compare the test statistic to the critical value
p_value <- 2 * pt(-abs(t_stat), 1000)
p_value <- rep(c(p_value, NA), each = 2)
p_value <- p_value[1:(length(p_value) - 2)]
# Set every second value to NA using a loop,such that only for YES we see the test result:
for (i in seq(1, length(p_value), by = 2)) {
  p_value[i] <- NA
}
#Add p-value to data:
data = cbind(data, p_value)

# Define the significance levels and corresponding labels
significance_labels <- c("(***)" = 0.01, "(**)" = 0.05, "(*)" = 0.10)

# Create a new column for significance labels based on p-value
data$Significance <- factor(
         ifelse(data$p_value < 0.05, "(**)",
                ifelse(data$p_value < 0.10, "(*)", "")),
  levels = names(significance_labels)
)

#Legend label:
leg <- "More than 15 GP visits in the year prior to the trial:"

##Laver loop over experiments:
experiments <- unique(data$Experiment)

for (experiment in experiments) {
  
  # Plotter N05:
  data_new = subset(data, Variable == "N05" & Experiment==experiment)
  plot_N05 <- ggplot(data_new, aes(x = Time, y = ATE, fill = Use, group = Use)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin = ATE - 1.96 * SE, ymax = ATE + 1.96 * SE), position = position_dodge(width = 0.9), width = 0.25) +
    labs(title = "", x = "", y = "Defined Daily Doses (DDD)") +
    scale_fill_grey(start = 0.5, end = 0.85) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5, vjust = 0.5),  # Adjust size here
      axis.text.y = element_text(size = 16),  # Adjust size here
      axis.title.x = element_text(size = 16),  # Adjust size here
      axis.title.y = element_text(size = 16),  # Adjust size here
      legend.text = element_text(size = 17),  # Adjust size here
      legend.title = element_text(size = 18)
    ) +
    guides(fill = guide_legend(title = leg)) +
    # Add significance indicators using geom_text
    geom_text(
      aes(label = Significance, y = max(abs(data_new$ATE) + 2 * data_new$SE)),
      position = position_dodge(width = 0),
      size = 6, vjust = 0, hjust=0.5
    ) 
  ggsave(paste("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/", experiment, "/N05_HTE_GP_services.pdf", sep = ""), plot_N05, device = "pdf", width = 12, height = 8)
  
  
  # Plotter N06:
  data_new = subset(data, Variable == "N06" & Experiment==experiment)
  plot_N06 <- ggplot(data_new, aes(x = Time, y = ATE, fill = Use, group = Use)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin = ATE - 1.96 * SE, ymax = ATE + 1.96 * SE), position = position_dodge(width = 0.9), width = 0.25) +
    labs(title = "", x = "", y = "Defined Daily Doses (DDD)") +
    scale_fill_grey(start = 0.5, end = 0.85) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5, vjust = 0.5),  # Adjust size here
      axis.text.y = element_text(size = 16),  # Adjust size here
      axis.title.x = element_text(size = 16),  # Adjust size here
      axis.title.y = element_text(size = 16),  # Adjust size here
      legend.text = element_text(size = 17),  # Adjust size here
      legend.title = element_text(size = 18)
    ) +
    guides(fill = guide_legend(title = leg)) +
    # Add significance indicators using geom_text
    geom_text(
      aes(label = Significance, y =max(abs(data_new$ATE) + 2 * data_new$SE)),
      position = position_dodge(width = 0),
      size = 6, vjust = 0, hjust=0.5
    ) 
  ggsave(paste("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/", experiment, "/N06_HTE_GP_services.pdf", sep = ""), plot_N06, device = "pdf", width = 12, height = 8)
  
  # Plotter Earnings:
  data_new = subset(data, Variable == "Earnings" & Experiment==experiment)
  plot_Earnings <- ggplot(data_new, aes(x = Time, y = ATE, fill = Use, group = Use)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin = ATE - 1.96 * SE, ymax = ATE + 1.96 * SE), position = position_dodge(width = 0.9), width = 0.25) +
    labs(title = "", x = "", y = "US Dollars") +
    scale_fill_grey(start = 0.5, end = 0.85) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5, vjust = 0.5),  # Adjust size here
      axis.text.y = element_text(size = 16),  # Adjust size here
      axis.title.x = element_text(size = 16),  # Adjust size here
      axis.title.y = element_text(size = 16),  # Adjust size here
      legend.text = element_text(size = 17),  # Adjust size here
      legend.title = element_text(size = 18)
    ) +
    guides(fill = guide_legend(title = leg)) +
    # Add significance indicators using geom_text
    geom_text(
      aes(label = Significance, y = max(abs(data_new$ATE) + 2 * data_new$SE)),
      position = position_dodge(width = 0),
      size = 6, vjust = 0, hjust=0.5
    ) 
  ggsave(paste("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/", experiment, "/Earnings_HTE_GP_services.pdf", sep = ""), plot_Earnings, device = "pdf", width = 12, height = 8)
  
  
  # Plotter Self-sufficient:
  data_new = subset(data, Variable == "Self-sufficient" & Experiment==experiment)
  plot_Self <- ggplot(data_new, aes(x = Time, y = ATE, fill = Use, group = Use)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin = ATE - 1.96 * SE, ymax = ATE + 1.96 * SE), position = position_dodge(width = 0.9), width = 0.25) +
    labs(title = "", x = "", y = "Weeks") +
    scale_fill_grey(start = 0.5, end = 0.85) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5, vjust = 0.5),  # Adjust size here
      axis.text.y = element_text(size = 16),  # Adjust size here
      axis.title.x = element_text(size = 16),  # Adjust size here
      axis.title.y = element_text(size = 16),  # Adjust size here
      legend.text = element_text(size = 17),  # Adjust size here
      legend.title = element_text(size = 18)
    ) +
    guides(fill = guide_legend(title = leg)) +
    # Add significance indicators using geom_text
    geom_text(
      aes(label = Significance, y = max(abs(data_new$ATE) + 2 * data_new$SE)),
      position = position_dodge(width = 0),
      size = 6, vjust = 0, hjust=0.5
    ) 
  ggsave(paste("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/", experiment, "/Self_sufficient_HTE_GP_services.pdf", sep = ""), plot_Self, device = "pdf", width = 12, height = 8)
}












#################################
#Table of GATE
################################
rm(list = ls())

new_loop=c("Social assistance","Unemployment benefits 1","Unemployment benefits 2a","Unemployment benefits 2b","Unemployment benefits 2c")
for(loop in 1:length(new_loop)) {
  
# Load the data
file_path <- paste0("~/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/",new_loop[loop],"/GATE_table.xlsx")
data <- read_excel(file_path)
  
# Filter the data for the specified outcomes and sub-groups
filtered_data <- data[-c(4,5,15,16,26,27,37,38), ]

convert_to_numeric <- function(x) {
  as.numeric(as.character(x))
}

# Convert columns 2 to 9 to numeric using lapply with error handling
filtered_data[, 2:9] <- lapply(filtered_data[, 2:9], convert_to_numeric)

#Round to 3 decimals:
#filtered_data[, c(2,3,5,6,8,9)] <- round(filtered_data[, c(2,3,5,6,8,9)],3) 
filtered_data[, c(2,3,5,6,8,9)] <- apply(filtered_data[, c(2,3,5,6,8,9)], 2, function(x) formatC(x, format = "f", digits = 3))
convert_to_numeric <- function(x) {
  as.numeric(as.character(x))
}

#Add significans stars:
add_significance_stars <- function(estimate, p_value) {
  if (is.na(p_value)) {
    return(as.character(estimate))
  }
  
  stars <- if (p_value < 0.05) {
    "**"
  } else if (p_value < 0.10) {
    "*"
  } else {
    ""
  }
  
  return(paste0(estimate, stars))
}
filtered_data$estimate...2 <- mapply(add_significance_stars, filtered_data$estimate...2, filtered_data$p_value...4)
filtered_data$estimate...5 <- mapply(add_significance_stars, filtered_data$estimate...5, filtered_data$p_value...7)
filtered_data$estimate...8 <- mapply(add_significance_stars, filtered_data$estimate...8, filtered_data$p_value...10)
filtered_data <- filtered_data[, -c(4, 7, 10)]

# Function to add parentheses around a number
add_parentheses <- function(x) {
  if (is.na(x)) {
    return(NA)
  } else {
    return(paste0("(", x, ")"))
  }
}
# Apply the function to std.err...3 column
filtered_data$std.err...3 <- sapply(filtered_data$std.err...3, add_parentheses)
filtered_data$std.err...6 <- sapply(filtered_data$std.err...6, add_parentheses)
filtered_data$std.err...9 <- sapply(filtered_data$std.err...9, add_parentheses)


#Gemmer til excel:
write.xlsx(filtered_data, file = paste("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/", new_loop[loop], "/HTE_table_main.xlsx", sep = ""), sheetName = "HTE results")
}


  


  
  
#####################MAKE PNAS PUBLICATION FIGURES:
####FIGURE 1:
# Convert PDF pages to images
earnings_image <- image_read_pdf("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/Earnings_ATE_main.pdf", pages = 1)
self_sufficient_image <- image_read_pdf("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/Self_sufficient_ATE_main.pdf", pages = 1)
# Function to create right-aligned label images with larger canvas and centered text
create_centered_label_image <- function(text) {
  # Set temporary file path
  temp_path <- tempfile(fileext = ".png")
  
  # Create a blank plot with more width for longer text display
  png(temp_path, width = 2500, height = 150)  # Increased width for more text space
  par(mar = c(0, 0, 0, 0), xpd = TRUE)  # xpd allows plotting outside default region
  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")  # Create a blank plot
  
  # Center-align text horizontally in the expanded canvas
  text(1.05, 1, labels = text, cex = 7, font = 3, adj = 0.5)  # Centered text, larger font
  
  dev.off()
  
  # Read the generated image
  label_image <- image_read(temp_path)
  
  # Remove the temporary file
  unlink(temp_path)
  
  return(label_image)
}

# Create centered label images with larger canvas
label_a <- create_centered_label_image("Panel A: Earnings (US dollars)")
label_b <- create_centered_label_image("Panel B: Non-receipt of benefits (weeks)")

# Append labels directly below each respective image
panel_a_with_label <- image_append(c(earnings_image, label_a), stack = TRUE)
panel_b_with_label <- image_append(c(self_sufficient_image, label_b), stack = TRUE)

# Combine the final images with labels side by side
final_combined_image <- image_append(c(panel_a_with_label, panel_b_with_label))


# Save the output as a PDF
image_write(final_combined_image, path = "/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/Long term health and unempl effects paper/Paper/Endelige filer/Figure 1.pdf", format = "pdf")








####FIGURE 2:
# Convert PDF pages to images
N05_image <- image_read_pdf("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/N05_ATE_main.pdf", pages = 1)
N06_image <- image_read_pdf("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/N06_ATE_main.pdf", pages = 1)
# Function to create right-aligned label images with larger canvas and centered text
create_centered_label_image <- function(text) {
  # Set temporary file path
  temp_path <- tempfile(fileext = ".png")
  
  # Create a blank plot with more width for longer text display
  png(temp_path, width = 2500, height = 150)  # Increased width for more text space
  par(mar = c(0, 0, 0, 0), xpd = TRUE)  # xpd allows plotting outside default region
  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")  # Create a blank plot
  
  # Center-align text horizontally in the expanded canvas
  text(1.05, 1, labels = text, cex = 7, font = 3, adj = 0.5)  # Centered text, larger font
  
  dev.off()
  
  # Read the generated image
  label_image <- image_read(temp_path)
  
  # Remove the temporary file
  unlink(temp_path)
  
  return(label_image)
}

# Create centered label images with larger canvas
label_a <- create_centered_label_image("Panel A: N05 (DDD)")
label_b <- create_centered_label_image("Panel B: N06 (DDD)")

# Append labels directly below each respective image
panel_a_with_label <- image_append(c(N05_image, label_a), stack = TRUE)
panel_b_with_label <- image_append(c(N06_image, label_b), stack = TRUE)

# Combine the final images with labels side by side
final_combined_image <- image_append(c(panel_a_with_label, panel_b_with_label))


# Save the output as a PDF
image_write(final_combined_image, path = "/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/Long term health and unempl effects paper/Paper/Endelige filer/Figure 2.pdf", format = "pdf")



####FIGURE 3:
# Convert PDF pages to images
earnings_image <- image_read_pdf("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/Unemployment benefits 1/Earnings_HTE_used_NO5orN06.pdf", pages = 1)
self_sufficient_image <- image_read_pdf("/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/results/R&R/Figures/Unemployment benefits 1/Self_sufficient_HTE_used_NO5orN06.pdf", pages = 1)
# Function to create right-aligned label images with larger canvas and centered text
create_centered_label_image <- function(text) {
  # Set temporary file path
  temp_path <- tempfile(fileext = ".png")
  
  # Create a blank plot with more width for longer text display
  png(temp_path, width = 2800, height = 150)  # Increased width for more text space
  par(mar = c(0, 0, 0, 0), xpd = TRUE)  # xpd allows plotting outside default region
  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")  # Create a blank plot
  
  # Center-align text horizontally in the expanded canvas
  text(1.15, 1, labels = text, cex = 7, font = 3, adj = 0.5)  # Centered text, larger font
  
  dev.off()
  
  # Read the generated image
  label_image <- image_read(temp_path)
  
  # Remove the temporary file
  unlink(temp_path)
  
  return(label_image)
}

# Create centered label images with larger canvas
label_a <- create_centered_label_image("Panel A: Earnings (US dollars)")
label_b <- create_centered_label_image("Panel B: Non-receipt of benefits (weeks)")

# Append labels directly below each respective image
panel_a_with_label <- image_append(c(earnings_image, label_a), stack = TRUE)
panel_b_with_label <- image_append(c(self_sufficient_image, label_b), stack = TRUE)

# Combine the final images with labels side by side
final_combined_image <- image_append(c(panel_a_with_label, panel_b_with_label))


# Save the output as a PDF
image_write(final_combined_image, path = "/Users/au304147/Library/CloudStorage/Dropbox/health_effects_rct/Long term health and unempl effects paper/Paper/Endelige filer/Figure 3.pdf", format = "pdf")



