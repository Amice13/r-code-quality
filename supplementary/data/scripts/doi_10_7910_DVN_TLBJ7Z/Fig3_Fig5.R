#Sharif Amlani
#R 4.1.1
#Winter 2021

######################## Code Summary ##################
# This code creates Figure 3 and 5 in the manuscript.

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
library(stringr)
library(textstem)

######################## Upload Data ##################

#Set Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Upload Data
load("Replication Data - Amlani and Kiesel 2024.rda"); Demographics.1 <- Demographics.Pure

######################## Subset Data ##################
Demographics.2 <- subset(Demographics.1, Check_Master_NoCon == "Check")

######################## Most Popular Words: Outgroup ##################

# Clean Words
Demographics.2$Word_R_clean <- gsub('[[:punct:] ]+',' ',trimws(tolower(textstem::lemmatize_words(Demographics.2$r_word))))
Demographics.2$Word_D_clean <- gsub('[[:punct:] ]+',' ',trimws(tolower(textstem::lemmatize_words(Demographics.2$d_word))))

#Add Frequency
Demographics.2$Freq <- 1

Top_Responces <- 10

#Find Most Popular (Democrat -> Republican)
Word_D_to_R_DF <- 
  setNames(
  plyr::join_all(list(
  aggregate(Freq ~ Word_R_clean, data = subset(Demographics.2, PID_3 == "Democrat"), sum), 
  aggregate(r_word_code_numeric ~ Word_R_clean,data = subset(Demographics.2, PID_3 == "Democrat"), mean)),
  by = c("Word_R_clean")),
  c("Word", "Freq", "Mean"))

Word_D_to_R_DF$Percent <- (Word_D_to_R_DF$Freq / sum(Word_D_to_R_DF$Freq))*100
Word_D_to_R_DF.2 <- Word_D_to_R_DF[order(-Word_D_to_R_DF$Percent),]
Word_D_to_R_DF.2$Group <- "Democrats about Republicans"

Word_D_to_R_DF_Plot <- head(Word_D_to_R_DF.2, Top_Responces)

#Find Most Popular (Republican -> Democrat)

Word_R_to_D_DF <- 
  setNames(
  plyr::join_all(list(
  aggregate(Freq ~ Word_D_clean, data = subset(Demographics.2, PID_3 == "Republican"), sum), 
  aggregate(d_word_code_numeric ~ Word_D_clean,data = subset(Demographics.2, PID_3 == "Republican"), mean)),
  by = c("Word_D_clean")),
  c("Word", "Freq", "Mean"))


Word_R_to_D_DF$Percent <- (Word_R_to_D_DF$Freq / sum(Word_R_to_D_DF$Freq))*100
Word_R_to_D_DF.2 <- Word_R_to_D_DF[order(-Word_R_to_D_DF$Percent),]
Word_R_to_D_DF.2$Group <- "Republicans about Democrats"

Word_R_to_D_DF_Plot <- head(Word_R_to_D_DF.2, Top_Responces)

#Rbind

Word_Out_Group.1 <- rbind(Word_D_to_R_DF_Plot, Word_R_to_D_DF_Plot)

######################## Most Popular Words: Ingroup ##################


#Find Most Popular (Democrat -> Democrat)
Word_D_to_D_DF <- 
  setNames(
    plyr::join_all(list(
      aggregate(Freq ~ Word_D_clean, data = subset(Demographics.2, PID_3 == "Democrat"), sum), 
      aggregate(d_word_code_numeric ~ Word_D_clean,data = subset(Demographics.2, PID_3 == "Democrat"), mean)),
      by = c("Word_D_clean")),
    c("Word", "Freq", "Mean"))

Word_D_to_D_DF$Percent <- (Word_D_to_D_DF$Freq / sum(Word_D_to_D_DF$Freq))*100
Word_D_to_D_DF.2 <- Word_D_to_D_DF[order(-Word_D_to_D_DF$Percent),]
Word_D_to_D_DF.2$Group <- "Democrats about Democrats"

Word_D_to_D_DF_Plot <- head(Word_D_to_D_DF.2, Top_Responces)


#Find Most Popular (Republican -> Republican)
Word_R_to_R_DF <- 
  setNames(
    plyr::join_all(list(
      aggregate(Freq ~ Word_R_clean, data = subset(Demographics.2, PID_3 == "Republican"), sum), 
      aggregate(r_word_code_numeric ~ Word_R_clean,data = subset(Demographics.2, PID_3 == "Republican"), mean)),
      by = c("Word_R_clean")),
    c("Word", "Freq", "Mean"))


Word_R_to_R_DF$Percent <- (Word_R_to_R_DF$Freq / sum(Word_R_to_R_DF$Freq))*100
Word_R_to_R_DF.2 <- Word_R_to_R_DF[order(-Word_R_to_R_DF$Percent),]
Word_R_to_R_DF.2$Group <- "Republicans about Republicans"

Word_R_to_R_DF_Plot <- head(Word_R_to_R_DF.2, Top_Responces)


#Rbind

Word_In_Group.1 <- rbind(Word_D_to_D_DF_Plot, Word_R_to_R_DF_Plot)


######################## Plot ##################
#********************** Out Group ********************
#Outgroup

Word_Out_Group.1$Word <- str_to_title(factor(Word_Out_Group.1$Word))

P1 <- ggplot(Word_Out_Group.1,
       aes( x= tidytext::reorder_within(Word, Freq,Group), y = Percent/100, fill = Group, label = round(Mean,1))) +
  geom_bar(stat="identity") +
  facet_wrap(Group ~. ,  scales = "free") +
  coord_flip() +
  tidytext::scale_x_reordered() +
  scale_fill_manual(values = c("#F44336", "#03A9F4")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_label(aes(label = round(Mean,2), alpha = abs(Mean)),colour = "black", nudge_y = -0.003) +
  labs(x = "One-Word Responses", y = "Percent Share of Responses in Each Group", title = "Most Frequent One-Word Outgroup Responses",   subtitle = paste("Top",Top_Responces, "One-Word Responses", sep = " "),
       caption = "Bars report the percentage of the total out-group responces.\nText reports mean self-reported code for each word.") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(fill = 'white', color = 'white'),
        legend.position='none',
        plot.caption = element_text(hjust = 0)); P1


#Ingroup
Word_In_Group.1$Word <- str_to_title(factor(Word_In_Group.1$Word))

P2 <- ggplot(Word_In_Group.1,
       aes( x= tidytext::reorder_within(Word, Freq,Group), y = Percent/100, fill = Group, label = round(Mean,1))) +
  geom_bar(stat="identity") +
  facet_wrap(Group ~. ,  scales = "free") +
  coord_flip() +
  tidytext::scale_x_reordered() +
  scale_fill_manual(values = c( "#03A9F4", "#F44336")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_label(aes(label = round(Mean,2), alpha = abs(Mean)),colour = "black", nudge_y = -0.003) +
  labs(x = "One-Word Responses", y = "Percent Share of Responses in Each Group", title = "Most Frequent One-Word In Group Responses",  subtitle = paste("Top",Top_Responces, "One-Word Responses", sep = " "),
       caption = "Bars report the percentage of the total in-group responces.\nText reports mean self-reported code for each word.") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(fill = 'white', color = 'white'),
        legend.position='none',
        plot.caption = element_text(hjust = 0)); P2


############################## Save ############################## 
ggsave(P1, 
       file = "Fig5.png",
       width=9, height=6,  dpi = 300)

ggsave(P2, 
       file = "Fig3.png",
       width=9, height=6,  dpi = 300)



