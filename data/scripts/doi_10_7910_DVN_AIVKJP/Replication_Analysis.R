rm(list=ls())

library(readxl)
library(tidyverse)
library(ggplot2)
library(maps)
library(kableExtra)

# set your working directory 

# define colorblind-friendly colors
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# load replication data 
load("data_replication.Rda")

# findings, overall description: 

# display over time. submissions and decisions  
# set up submission and decision counts 
submission_counts <- data %>%
  group_by(year = year(communication)) %>% 
  summarise(submission_count = n(), .groups = "drop")

decision_counts <- data %>%
  group_by(year = year(decision)) %>% 
  summarise(decision_count = n(), .groups = "drop")

### put decisions and submissions in one data frame ### 
time <- merge(submission_counts,decision_counts, by="year")

# figure 1: submissions and decisions over time 
time_plot <- ggplot(time, aes(x = year)) + 
  geom_line(aes(y = submission_count, color = "Submissions"), size = 1) + 
  geom_line(aes(y = decision_count, color = "Decisions"), size = 1) +
  scale_color_manual(values = cbPalette) +
  labs(
    title ="Submissions and Decisions over Time",
    x = "Year",
    y = "Count",
    color = "Type") + 
  theme_minimal()
time_plot

# across countries 
unique(data$State)
states <- 
  data %>% count(State)
states 

# Sort the data in descending order by value
states <- states[order(-states$n), ]
states$State <- factor(states$State, levels = states$State[order(-states$n)])

nrow(states)

# figure 2: submissions against OPIC States Parties 
countries <- ggplot(states, aes(x = State, y = n)) +
  geom_bar(stat = "identity") +
  theme_minimal() + 
  labs(title = "Submissions against OPIC States Parties",
       y= "Number of Submissions", x = "State Party") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for better visibility 
countries


### section: who submits? ####
#### see separate .R file for DOBs and age (fig 3)#

# representation 
data$representation <- as.numeric(data$`Representation?`)
sum(data$representation, na.rm = TRUE)
sum(data$representation, na.rm = TRUE)/ nrow(data)
nrow(data) -sum(data$representation, na.rm = TRUE)
norep <- data[data$representation==0,]
nrow(norep)
unique(norep$Outcome)

# nationality 
barplot(sort(table(data$`Nationality of Victim`),decreasing=T),las=2)
barplot(sort(table(data$`Nationality of Authors (if author is not victim)`),decreasing=T),las=2)


### section: what are petitions about? ####
# migration 
data$migration <- as.numeric(data$`Concerns Migration?`)
sum(data$migration, na.rm = TRUE)
sum(data$migration, na.rm = TRUE)/ nrow(data)

sum(data$age_verification)
sum(data$State=="Spain")/nrow(data)
sum(data$age_verification & data$State=="Spain")
sum(data$age_verification & data$State=="Spain")/nrow(data)

### section: committee decisions ####

# figure 4 subfigure: outcomes over time (4a)
plot_four_bar <- ggplot(data, aes(x = decision_year, fill = Outcome)) +
  geom_bar(stat = "count", position = "dodge") +  # Bar plot
  facet_wrap(~ Outcome) +  # Create a separate plot for each outcome
  #geom_point(size = 2) + 
  labs(x = "Year", y = "Count", 
       #title = "Outcomes over Time"
       ) +
  theme_minimal() +
  scale_fill_manual(values = cbPalette)
plot_four_bar

table(data$Outcome)

# inadmissible
sum(data$Outcome=="Inadmissible")
sum(data$Outcome=="Inadmissible") / (max(data$decision_year)-min(data$decision_year))

# discontinuance 
dis <- data[data$Outcome=="Discontinuance",]
table(dis$`Outcome Good, Bad, or Unknown`)
sum(dis$`Outcome Good, Bad, or Unknown`=="Good")
sum(dis$`Outcome Good, Bad, or Unknown`=="Good")/nrow(dis)

# good discontinuance over time 
dis$type <- NA
dis$type[dis$`Outcome Good, Bad, or Unknown`=="Good"] <- "good"
dis_counts <- dis %>%
  group_by(year = year(decision), type) %>% 
  summarise(count = n(), .groups = "drop")

# figure 4 subfigure: discontinuances over time (4b)
dis_plot <- ggplot(dis_counts, aes(x = year, y = count, color = type)) +
  geom_line() + 
  labs(title = "Discontinuances over Time",
       x = "Year", y = "Count") +
  scale_color_manual(values = "#E69F00", labels = c("Favorable","Unfavorable"), name ="Type")+ 
  theme_minimal() + 
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 14),     # Bigger legend title
    legend.text = element_text(size = 12),      # Bigger legend labels
    legend.key.size = unit(1.2, "cm")           # Bigger legend boxes
  )
dis_plot

# favorable outcomes over time 
data$dis_good <- 0 
data$dis_good[dis$`Outcome Good, Bad, or Unknown`=="Good"] <- 1 

data$dis_bad <- 0
data$dis_bad[data$Outcome=="Discontinuance" & 
               data$`Outcome Good, Bad, or Unknown`!="Good"] <- 1
data$violation <- 0 
data$violation[data$Outcome=="Violation"] <- 1
data$noviolation <- 0 
data$noviolation[data$Outcome==" No Violation"] <- 1

data$favorable <- 0 
data$favorable[data$violation==1 | data$dis_good==1] <- 1
data$unfav <- 0 
data$unfav[data$noviolation==1 | data$dis_bad==1] <- 1

sum(data$favorable)
sum(data$favorable)/nrow(data)

data_summary <- data %>%
  group_by(year = year(decision)) %>%
  summarise(
    favorable = sum(favorable, na.rm = TRUE),
    unfav = sum(unfav, na.rm = TRUE)
  )

data_long <- data_summary %>%
  pivot_longer(cols = c(favorable, unfav), 
               names_to = "type", 
               values_to = "count")

# figure 4 subplot: favorable outcomes over time (4c)
plot_favorable <- ggplot(data_long, aes(x= year, y = count, fill = type)) +
  geom_col(position = position_dodge(width = 0.9),width = 0.8) +
  labs(title = "Favorable Outcomes over Time",
       x = "Year", 
       y = "Count", 
       fill = "Type") +
  scale_fill_manual(values = c("#E69F00","#999999"), labels = c("Favorable","Unfavorable"))+ 
  theme_minimal() + 
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 14),     # Bigger legend title
    legend.text = element_text(size = 12),      # Bigger legend labels
    legend.key.size = unit(1.2, "cm")           # Bigger legend boxes
  )
plot_favorable


