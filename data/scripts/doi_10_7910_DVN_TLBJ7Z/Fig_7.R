#Sharif Amlani
#R 4.1.1
#Fall 2021


######################## Code Summary ##################
#This code creates Figure 7 in the manuscript. 

#********************* Notes ***********************
#Measures of Affect

#Democrats
#therm_1_2
#Biden_Evaluation
#D_Lifestyle
#d_word_code_numeric
#d_word2_code_numeric

#Republicans
#therm_1_1
#Trump_Evaluation
#R_Lifestyle
#r_word_code_numeric
#r_word2_code_numeric
########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 3)
set.seed(1993)

######################### Functions ###################

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}


######################### Library #####################
library(psych)
library(reshape2)
library(ggplot2)
######################## Upload Data ##################

#********************* Demographics *******************
#Set Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Upload Data
load("Replication Data - Amlani and Kiesel 2024.rda"); Demographics.1 <- Demographics.Pure


####################### Examine Data ########################
head(Demographics.1)

################## Respondents Who Pass Checks ########################
Demographics.2 <- subset(Demographics.1, Check_Master_NoCon == "Check")

################## Subset Measures ########################
#Democratic Evaluations
Demographics.D.1 <- with(Demographics.2, data.frame(Thermometer = as.numeric(therm_1_2), Biden_Evaluation, D_Lifestyle, d_word_code_numeric, d_word2_code_numeric))

#Republican Evaluations
Demographics.R.1 <- with(Demographics.2, data.frame(Thermometer = as.numeric(therm_1_1), Trump_Evaluation, R_Lifestyle, r_word_code_numeric, r_word2_code_numeric))


################## Scatterplot ########################
#********************* Democrat ************************

Plot_D_1 <- ggplot(Demographics.D.1, aes (x = d_word_code_numeric, y = Thermometer)) +
  geom_jitter(width = 0.25, color = "#03A9F4", alpha = 0.2) +
  stat_smooth(method = "lm", color = "black") +
  theme_minimal() +
  labs(x = "Self-Coded One Word Evaluations",
       y = "Feeling Thermometer",
       subtitle = paste("One Word vs. Feeling Thermometer\nTotal Correlation = ", round(cor(Demographics.D.1$Thermometer, Demographics.D.1$d_word_code_numeric, use = "complete"), 2), sep = "")) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption= element_text(hjust = 0),
        legend.position = "bottom")

Plot_D_2 <- ggplot(Demographics.D.1, aes (x = d_word_code_numeric, y = Biden_Evaluation)) +
  geom_jitter(width = 0.25, color = "#03A9F4", alpha = 0.2) +
  stat_smooth(method = "lm", color = "black") +
  theme_minimal() +
  labs(x = "Self-Coded One Word Evaluations",
       y = "Joe Biden Candidate Evaluation",
       title = "Democratic Affect Evaluations",
       subtitle = paste("One Word vs. Biden Evaluation\nTotal Correlation = ", round(cor(Demographics.D.1$Biden_Evaluation, Demographics.D.1$d_word_code_numeric, use = "complete"), 2), sep = "")) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption= element_text(hjust = 0),
        legend.position = "bottom")

Plot_D_3 <- ggplot(Demographics.D.1, aes (x = d_word_code_numeric, y = D_Lifestyle)) +
  geom_jitter(width = 0.25, color = "#03A9F4", alpha = 0.2) +
  stat_smooth(method = "lm", color = "black") +
  theme_minimal() +
  labs(x = "Self-Coded One Word Evaluations",
       y = "Social-Distance Evaluation",
       subtitle = paste("One Word vs. Social-Distance Evaluation\nTotal Correlation = ", round(cor(Demographics.D.1$D_Lifestyle, Demographics.D.1$d_word_code_numeric, use = "complete"), 2), sep = "")) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption= element_text(hjust = 0),
        legend.position = "bottom")

#********************* Republican ************************
Plot_R_1 <- ggplot(Demographics.R.1, aes (x = r_word_code_numeric, y = Thermometer)) +
  geom_jitter(width = 0.25, color = "#F44336", alpha = 0.2) +
  stat_smooth(method = "lm", color = "black") +
  theme_minimal() +
  labs(x = "Self-Coded One Word Evaluations",
       y = "Feeling Thermometer",
       subtitle = paste("One Word vs. Feeling Thermometer\nTotal Correlation = ", round(cor(Demographics.R.1$Thermometer, Demographics.R.1$r_word_code_numeric, use = "complete"), 2), sep = "")) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption= element_text(hjust = 0),
        legend.position = "bottom")

Plot_R_2 <- ggplot(Demographics.R.1, aes (x = r_word_code_numeric, y = Trump_Evaluation)) +
  geom_jitter(width = 0.25, color = "#F44336", alpha = 0.2) +
  stat_smooth(method = "lm", color = "black") +
  theme_minimal() +
  labs(x = "Self-Coded One Word Evaluations",
       y = "Donald Trump Candidate Evaluation",
       title = "Republican Affect Evaluations",
       subtitle = paste("One Word vs. Trump Evaluation\nTotal Correlation = ", round(cor(Demographics.R.1$Trump_Evaluation, Demographics.R.1$r_word_code_numeric, use = "complete"), 2), sep = "")) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption= element_text(hjust = 0),
        legend.position = "bottom")

Plot_R_3 <- ggplot(Demographics.R.1, aes (x = r_word_code_numeric, y = R_Lifestyle)) +
  geom_jitter(width = 0.25, color = "#F44336", alpha = 0.2) +
  stat_smooth(method = "lm", color = "black") +
  theme_minimal() +
  labs(x = "Self-Coded One Word Evaluations",
       y = "Social-Distance Evaluation",
       subtitle =  paste("One Word vs. Social-Distance Evaluation\nTotal Correlation = ", round(cor(Demographics.R.1$R_Lifestyle, Demographics.R.1$r_word_code_numeric, use = "complete"), 2), sep = "")) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption= element_text(hjust = 0),
        legend.position = "bottom")

#################### Patchwork ###################
library(patchwork)

patchwork <- (Plot_D_1 + Plot_D_2 + Plot_D_3) / (Plot_R_1 + Plot_R_2 + Plot_R_3)

patchwork_final <- patchwork + plot_annotation(caption = 'Scatterplot reports self-coded one-word evaluations across feeling thermometers, candidate evaluations, and social-distance questions.\nAffect about Democrats (Republicans) reported in the upper (lower) panel.\nPlots fitted with a linear line with 95% confidence intervals.\nPoints are jittered.') &
  theme(plot.caption= element_text(hjust = 0))

patchwork_final


#################### Save ###################

ggsave(patchwork_final, 
       file = "Fig7.png",
       width=10, height=8,  dpi = 300)
