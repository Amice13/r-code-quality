#Sharif Amlani
#R 4.1.1
#Winter 2021

######################## Code Summary ##################
# This code creates Figure 2, 4, and 6 in the manuscript.
########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 3)
set.seed(1993)

######################### Functions ###################
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
######################### Library #####################
library(ggplot2)
library(patchwork)
library(textstem)
library(ggridges)

######################## Upload Data ##################

#Set Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Upload Data
load("Replication Data - Amlani and Kiesel 2024.rda"); Demographics.1 <- Demographics.Pure

######################## Subset Data ##################
Demographics.2 <- subset(Demographics.1, Check_Master_NoCon == "Check")

###################### In/Out Group Feelings #######################

# In Group Feelings

summary(as.numeric(Demographics.2$d_word_code_numeric[Demographics.2$PID_3 == "Democrat" & !is.na(Demographics.2$PID_3)]))
summary(as.numeric(Demographics.2$r_word_code_numeric[Demographics.2$PID_3 == "Republican" & !is.na(Demographics.2$PID_3)]))

# Out Group Feelings

summary(as.numeric(Demographics.2$d_word_code_numeric[Demographics.2$PID_3 == "Republican" & !is.na(Demographics.2$PID_3)]))
summary(as.numeric(Demographics.2$r_word_code_numeric[Demographics.2$PID_3 == "Democrat" & !is.na(Demographics.2$PID_3)]))

#************************ Data Management ************************ 
Demographics.2$In_Group_Code <- ifelse(Demographics.2$PID_3 == "Democrat", Demographics.2$d_word_code_numeric, NA)
Demographics.2$In_Group_Code <- ifelse(Demographics.2$PID_3 == "Republican", Demographics.2$r_word_code_numeric, Demographics.2$In_Group_Code)

Demographics.2$Out_Group_Code <- ifelse(Demographics.2$PID_3 == "Democrat", Demographics.2$r_word_code_numeric, NA)
Demographics.2$Out_Group_Code <- ifelse(Demographics.2$PID_3 == "Republican", Demographics.2$d_word_code_numeric, Demographics.2$Out_Group_Code)

Plot_DF.1 <- reshape2::melt(with(subset(Demographics.2, PID_3 %in% c("Democrat", "Republican")), data.frame(PID_3, In_Group_Code, Out_Group_Code)))

Plot_DF.1$IV[Plot_DF.1$PID_3 == "Republican" & Plot_DF.1$variable == "In_Group_Code"] <- "Republicans Sentiment Towards Republicans"
Plot_DF.1$IV[Plot_DF.1$PID_3 == "Republican" & Plot_DF.1$variable == "Out_Group_Code"]<- "Republicans Sentiment Towards Democrats"
Plot_DF.1$IV[Plot_DF.1$PID_3 == "Democrat" & Plot_DF.1$variable == "In_Group_Code"]<- "Democrats Sentiment Towards Democrats"
Plot_DF.1$IV[Plot_DF.1$PID_3 == "Democrat" & Plot_DF.1$variable == "Out_Group_Code"]<- "Democrats Sentiment Towards Republicans"


#************************ Ridgeplot: In/Out Group Feelings ************************ 

Plot_DF.1$IV <- factor(Plot_DF.1$IV, levels = rev(c("Republicans Sentiment Towards Republicans",
                                                    "Democrats Sentiment Towards Democrats",
                                                    "Republicans Sentiment Towards Democrats",
                                                    "Democrats Sentiment Towards Republicans")))


#Ridgeplot
P6a <- ggplot(Plot_DF.1, aes(x = value, y = IV, fill = IV)) + 
  stat_density_ridges(quantile_lines = TRUE, quantiles = c(0.5 ))+
  theme_minimal()+
  scale_fill_manual(values = rev(c("#F44336", "#03A9F4", "#EF9A9A", "#81D4FA"))) +
  labs(title = "Respondents' Self Coded Word Ratings",
       x = "Self-Coded Word Value",
       caption = "Note: Plot reports the density distribution of affect between and across partisans.\nPositive (Negative) values represent positive (negative) affect.\nSolid line represents within-group median values.\nDashed line reports neutral selection (0).") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(size=10, hjust = 0.0, face="italic"), 
        legend.position="bottom",
        axis.title.y=element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.background = element_rect(fill = 'white', color = 'white')) +
  annotate("text", x = 2.5, y = 0.7, label = "Positive Affect") +
  annotate("text", x = -2.5, y = 0.7, label = "Negative Affect") +
  scale_x_continuous("Self-Coded Word Value", breaks = seq(-3, 3, 1), labels = seq(-3, 3, 1)) +
  guides(fill = "none")  +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey"); P6a


ggsave(P6a, 
       file = "Fig2.png",
       width=10, height=7,  dpi = 300)

#************************ Summary: In/Out Group Feelings ************************ 
aggregate(value ~ IV, Plot_DF.1, mean)
aggregate(value ~ IV, Plot_DF.1, median)
aggregate(value ~ IV, Plot_DF.1, summary)

#************************ Percentages ************************
#Percent Positive
Percent_Positive_Final <- NULL
for(i in unique(Plot_DF.1$IV)){
  DF.Percent <- data.frame(table(subset(Plot_DF.1, IV ==i)$value))
  DF.Percent$Var1 <- as.numeric(as.character(DF.Percent$Var1))
  Percent_Positive <- sum(subset(DF.Percent, Var1 > 0)$Freq) / sum(DF.Percent$Freq)
  
  Percent_Positive.1 <- data.frame(IV = i, Percent_Positive)
  Percent_Positive_Final <- rbind(Percent_Positive_Final, Percent_Positive.1)
}


Percent_Neutral_Final <- NULL
for(i in unique(Plot_DF.1$IV)){
  DF.Percent <- data.frame(table(subset(Plot_DF.1, IV ==i)$value))
  DF.Percent$Var1 <- as.numeric(as.character(DF.Percent$Var1))
  Percent_Neutral <- sum(subset(DF.Percent, Var1 == 0)$Freq) / sum(DF.Percent$Freq)
  
  Percent_Neutral.1 <- data.frame(IV = i, Percent_Neutral)
  Percent_Neutral_Final <- rbind(Percent_Neutral_Final, Percent_Neutral.1)
}


Percent_Negative_Final <- NULL
for(i in unique(Plot_DF.1$IV)){
  DF.Percent <- data.frame(table(subset(Plot_DF.1, IV ==i)$value))
  DF.Percent$Var1 <- as.numeric(as.character(DF.Percent$Var1))
  Percent_Negative <- sum(subset(DF.Percent, Var1 < 0)$Freq) / sum(DF.Percent$Freq)
  
  Percent_Negative.1 <- data.frame(IV = i, Percent_Negative)
  Percent_Negative_Final <- rbind(Percent_Negative_Final, Percent_Negative.1)
}


library(plyr)
Percent_Final <- plyr::join_all(list(Percent_Positive_Final, Percent_Neutral_Final, Percent_Negative_Final), by = "IV")

######################## Most Popular Words: Outgroup ##################
# Clean Words

Demographics.2$Word_R_clean <- gsub('[[:punct:] ]+',' ',trimws(tolower(textstem::lemmatize_words(Demographics.2$r_word))))
Demographics.2$Word_D_clean <- gsub('[[:punct:] ]+',' ',trimws(tolower(textstem::lemmatize_words(Demographics.2$d_word))))

#Find Most Popular (Democrat -> Republican)
Word_D_to_R_DF <- data.frame(table(subset(Demographics.2, PID_3 == "Democrat")$Word_R_clean))
Word_D_to_R_DF$Percent <- (Word_D_to_R_DF$Freq / sum(Word_D_to_R_DF$Freq))*100
Word_D_to_R_DF.2 <- Word_D_to_R_DF[order(-Word_D_to_R_DF$Percent),]
Word_D_to_R_DF.2$Group <- "Democrats about Republicans"

Word_D_to_R_DF_Plot <- head(Word_D_to_R_DF.2, 10)

#Find Most Popular (Republican -> Democrat)

Word_R_to_D_DF <- data.frame(table(subset(Demographics.2, PID_3 == "Republican")$Word_D_clean))
Word_R_to_D_DF$Percent <- (Word_R_to_D_DF$Freq / sum(Word_R_to_D_DF$Freq))*100
Word_R_to_D_DF.2 <- Word_R_to_D_DF[order(-Word_R_to_D_DF$Percent),]
Word_R_to_D_DF.2$Group <- "Republicans about Democrats"

Word_R_to_D_DF_Plot <- head(Word_R_to_D_DF.2, 10)

#Rbind
Word_Out_Group.1 <- rbind(Word_D_to_R_DF_Plot, Word_R_to_D_DF_Plot)

######################## Most Popular Words: Ingroup ##################

#Find Most Popular (Democrat -> Democrat)

Word_D_to_D_DF <- data.frame(table(subset(Demographics.2, PID_3 == "Democrat")$Word_D_clean))
Word_D_to_D_DF$Percent <- (Word_D_to_D_DF$Freq / sum(Word_D_to_D_DF$Freq))*100
Word_D_to_D_DF.2 <- Word_D_to_D_DF[order(-Word_D_to_D_DF$Percent),]
Word_D_to_D_DF.2$Group <- "Democrats about Democrats"

Word_D_to_D_DF_Plot <- head(Word_D_to_D_DF.2, 10)

#Find Most Popular (Republican -> Republican)

Word_R_to_R_DF <- data.frame(table(subset(Demographics.2, PID_3 == "Republican")$Word_R_clean))
Word_R_to_R_DF$Percent <- (Word_R_to_R_DF$Freq / sum(Word_R_to_R_DF$Freq))*100
Word_R_to_R_DF.2 <- Word_R_to_R_DF[order(-Word_R_to_R_DF$Percent),]
Word_R_to_R_DF.2$Group <- "Republicans about Republicans"

Word_R_to_R_DF_Plot <- head(Word_R_to_R_DF.2, 10)


#Rbind
Word_In_Group.1 <- rbind(Word_D_to_D_DF_Plot, Word_R_to_R_DF_Plot)


######################## Word Cloud ##################
library(ggwordcloud)
set.seed(42)

#****************** Out Group Evaluations ****************** 
P1 <- ggplot(head(Word_D_to_R_DF.2, 40), aes(label = Var1, size = Freq, color = Group)) +
  geom_text_wordcloud(area_corr = TRUE,eccentricity = 1, shape = "square") +
  theme_minimal() +
  scale_size_area(max_size = 50)  +
  labs(subtitle = "Democrats About Republicans") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position='none') +
  scale_color_manual(values = c("#F44336"))


P2 <- ggplot(head(Word_R_to_D_DF.2, 40), aes(label = Var1, size = Freq, color = Group)) +
  geom_text_wordcloud(area_corr = TRUE,eccentricity = 1, shape = "square") +
  theme_minimal() +
  scale_size_area(max_size = 50)  +
  labs(subtitle = "Republicans About Democrats") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position='none') +
  scale_color_manual(values = c("#03A9F4"))

patchwork <- P1 + P2

Out_Group_final <- patchwork + plot_annotation(title = "Most Frequent One-Word Outgroup Responses",
                                               caption = 'Note: Figure reports respondents one word to describing outpartisans.') &
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption= element_text(hjust = 0))

Out_Group_final

#Save
ggsave(Out_Group_final, 
       file = "Fig6.png",
       width=9, height=7,  dpi = 300)


#****************** In Group Evaluations ****************** 
P1 <- ggplot(head(Word_R_to_R_DF.2, 40), aes(label = Var1, size = Freq, color = Group)) +
  geom_text_wordcloud(area_corr = TRUE,eccentricity = 1, shape = "square") +
  theme_minimal() +
  scale_size_area(max_size = 50)  +
  labs(subtitle = "Republicans About Republicans") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position='none') +
  scale_color_manual(values = c("#F44336"))


P2 <- ggplot(head(Word_D_to_D_DF.2, 40), aes(label = Var1, size = Freq, color = Group)) +
  geom_text_wordcloud(area_corr = TRUE,eccentricity = 1, shape = "square") +
  theme_minimal() +
  scale_size_area(max_size = 50)  +
  labs(subtitle = "Democrats About Democrats") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position='none') +
  scale_color_manual(values = c("#03A9F4"))

patchwork <- P1 + P2

In_Group_final <- patchwork + plot_annotation(title = "Most Frequent One-Word In-Group Responses",
                                              caption = 'Note: Figure reports respondents one word to describing their own partisans.') &
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption= element_text(hjust = 0))

In_Group_final

#Save
ggsave(In_Group_final, 
       file = "Fig4.png",
       width=9, height=7,  dpi = 300)
