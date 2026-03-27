# Please set your working directory below and put the data files there.

setwd("")

install.packages("ggplot2")
install.packages("plyr")
install.packages("estimatr")
install.packages("scales")
install.packages("ltm")
install.packages("irtoys")

library(ggplot2)
library(plyr)
library(estimatr)
library(scales)
library(ltm)
library(irtoys)

rm (list = ls(all=TRUE))

#read baseline data file

student_b <- read.csv("Data_MadagascarIE_student_baseline.csv", header=T, stringsAsFactors = F)

#read end-line data file 

student_e <- read.csv("Data_MadagascarIE_student_endline.csv", header=T, stringsAsFactors = F,
                      fileEncoding = "CP932")

#Figure 1#####

student_b$Competency_level_Literacy_debutant <- 
  ifelse(student_b$Competency_level_Literacy=="DEBUTANT",1,0)

student_b$Competency_level_Literacy_lettre <- 
  ifelse(student_b$Competency_level_Literacy=="LETTRE",1,0)

student_b$Competency_level_Literacy_word <- 
  ifelse(student_b$Competency_level_Literacy=="MOT",1,0)

student_b$Competency_level_Literacy_paragraphe <- 
  ifelse(student_b$Competency_level_Literacy=="PARAGRAPHE",1,0)

student_b$Competency_level_Literacy_story <- 
  ifelse(student_b$Competency_level_Literacy=="HISTOIRE",1,0)

#all grades

student_all_b <- subset(student_b, Attendance_Numeracy==1&Attendance_Literacy==1)

student_all_b_t <- subset(student_all_b, treatment==1)
student_all_b_c <- subset(student_all_b, treatment==0)

#treatment

Grade_all_Literacy_debutant_rate_t <-
  sum(student_all_b_t$Competency_level_Literacy_debutant)/
  (as.integer(length(student_all_b_t$Sex)))

Grade_all_Literacy_lettre_rate_t <-
  sum(student_all_b_t$Competency_level_Literacy_lettre)/
  (as.integer(length(student_all_b_t$Sex)))

Grade_all_Literacy_word_rate_t <-
  sum(student_all_b_t$Competency_level_Literacy_word)/
  (as.integer(length(student_all_b_t$Sex)))

Grade_all_Literacy_paragraphe_rate_t <-
  sum(student_all_b_t$Competency_level_Literacy_paragraphe)/
  (as.integer(length(student_all_b_t$Sex)))

Grade_all_Literacy_story_rate_t <-
  sum(student_all_b_t$Competency_level_Literacy_story)/
  (as.integer(length(student_all_b_t$Sex)))

#Control

Grade_all_Literacy_debutant_rate_c <-
  sum(student_all_b_c$Competency_level_Literacy_debutant)/
  (as.integer(length(student_all_b_c$Sex)))

Grade_all_Literacy_lettre_rate_c <-
  sum(student_all_b_c$Competency_level_Literacy_lettre)/
  (as.integer(length(student_all_b_c$Sex)))

Grade_all_Literacy_word_rate_c <-
  sum(student_all_b_c$Competency_level_Literacy_word)/
  (as.integer(length(student_all_b_c$Sex)))

Grade_all_Literacy_paragraphe_rate_c <-
  sum(student_all_b_c$Competency_level_Literacy_paragraphe)/
  (as.integer(length(student_all_b_c$Sex)))

Grade_all_Literacy_story_rate_c <-
  sum(student_all_b_c$Competency_level_Literacy_story)/
  (as.integer(length(student_all_b_c$Sex)))

#grade 3

student_g3_b <- subset(student_b, Grade==3&Attendance_Numeracy==1&Attendance_Literacy==1)

student_g3_b_t <- subset(student_g3_b, treatment==1)
student_g3_b_c <- subset(student_g3_b, treatment==0)

#treatment

Grade3_Literacy_debutant_rate_t <-
  sum(student_g3_b_t$Competency_level_Literacy_debutant)/
  (as.integer(length(student_g3_b_t$Sex)))

Grade3_Literacy_lettre_rate_t <-
  sum(student_g3_b_t$Competency_level_Literacy_lettre)/
  (as.integer(length(student_g3_b_t$Sex)))

Grade3_Literacy_word_rate_t <-
  sum(student_g3_b_t$Competency_level_Literacy_word)/
  (as.integer(length(student_g3_b_t$Sex)))

Grade3_Literacy_paragraphe_rate_t <-
  sum(student_g3_b_t$Competency_level_Literacy_paragraphe)/
  (as.integer(length(student_g3_b_t$Sex)))

Grade3_Literacy_story_rate_t <-
  sum(student_g3_b_t$Competency_level_Literacy_story)/
  (as.integer(length(student_g3_b_t$Sex)))

#Control

Grade3_Literacy_debutant_rate_c <-
  sum(student_g3_b_c$Competency_level_Literacy_debutant)/
  (as.integer(length(student_g3_b_c$Sex)))

Grade3_Literacy_lettre_rate_c <-
  sum(student_g3_b_c$Competency_level_Literacy_lettre)/
  (as.integer(length(student_g3_b_c$Sex)))

Grade3_Literacy_word_rate_c <-
  sum(student_g3_b_c$Competency_level_Literacy_word)/
  (as.integer(length(student_g3_b_c$Sex)))

Grade3_Literacy_paragraphe_rate_c <-
  sum(student_g3_b_c$Competency_level_Literacy_paragraphe)/
  (as.integer(length(student_g3_b_c$Sex)))

Grade3_Literacy_story_rate_c <-
  sum(student_g3_b_c$Competency_level_Literacy_story)/
  (as.integer(length(student_g3_b_c$Sex)))

#grade 4

student_g4_b <- subset(student_b, Grade==4&Attendance_Numeracy==1&Attendance_Literacy==1)

student_g4_b_t <- subset(student_g4_b, treatment==1)
student_g4_b_c <- subset(student_g4_b, treatment==0)

#treatment

Grade4_Literacy_debutant_rate_t <-
  sum(student_g4_b_t$Competency_level_Literacy_debutant)/
  (as.integer(length(student_g4_b_t$Sex)))

Grade4_Literacy_lettre_rate_t <-
  sum(student_g4_b_t$Competency_level_Literacy_lettre)/
  (as.integer(length(student_g4_b_t$Sex)))

Grade4_Literacy_word_rate_t <-
  sum(student_g4_b_t$Competency_level_Literacy_word)/
  (as.integer(length(student_g4_b_t$Sex)))

Grade4_Literacy_paragraphe_rate_t <-
  sum(student_g4_b_t$Competency_level_Literacy_paragraphe)/
  (as.integer(length(student_g4_b_t$Sex)))

Grade4_Literacy_story_rate_t <-
  sum(student_g4_b_t$Competency_level_Literacy_story)/
  (as.integer(length(student_g4_b_t$Sex)))

#control

Grade4_Literacy_debutant_rate_c <-
  sum(student_g4_b_c$Competency_level_Literacy_debutant)/
  (as.integer(length(student_g4_b_c$Sex)))

Grade4_Literacy_lettre_rate_c <-
  sum(student_g4_b_c$Competency_level_Literacy_lettre)/
  (as.integer(length(student_g4_b_c$Sex)))

Grade4_Literacy_word_rate_c <-
  sum(student_g4_b_c$Competency_level_Literacy_word)/
  (as.integer(length(student_g4_b_c$Sex)))

Grade4_Literacy_paragraphe_rate_c <-
  sum(student_g4_b_c$Competency_level_Literacy_paragraphe)/
  (as.integer(length(student_g4_b_c$Sex)))

Grade4_Literacy_story_rate_c <-
  sum(student_g4_b_c$Competency_level_Literacy_story)/
  (as.integer(length(student_g4_b_c$Sex)))

#grade 5

student_g5_b <- subset(student_b, Grade==5&Attendance_Numeracy==1&Attendance_Literacy==1)

student_g5_b_t <- subset(student_g5_b, treatment==1)
student_g5_b_c <- subset(student_g5_b, treatment==0)

#treatment

Grade5_Literacy_debutant_rate_t <-
  sum(student_g5_b_t$Competency_level_Literacy_debutant)/
  (as.integer(length(student_g5_b_t$Sex)))

Grade5_Literacy_lettre_rate_t <-
  sum(student_g5_b_t$Competency_level_Literacy_lettre)/
  (as.integer(length(student_g5_b_t$Sex)))

Grade5_Literacy_word_rate_t <-
  sum(student_g5_b_t$Competency_level_Literacy_word)/
  (as.integer(length(student_g5_b_t$Sex)))

Grade5_Literacy_paragraphe_rate_t <-
  sum(student_g5_b_t$Competency_level_Literacy_paragraphe)/
  (as.integer(length(student_g5_b_t$Sex)))

Grade5_Literacy_story_rate_t <-
  sum(student_g5_b_t$Competency_level_Literacy_story)/
  (as.integer(length(student_g5_b_t$Sex)))

#control

Grade5_Literacy_debutant_rate_c <-
  sum(student_g5_b_c$Competency_level_Literacy_debutant)/
  (as.integer(length(student_g5_b_c$Sex)))

Grade5_Literacy_lettre_rate_c <-
  sum(student_g5_b_c$Competency_level_Literacy_lettre)/
  (as.integer(length(student_g5_b_c$Sex)))

Grade5_Literacy_word_rate_c <-
  sum(student_g5_b_c$Competency_level_Literacy_word)/
  (as.integer(length(student_g5_b_c$Sex)))

Grade5_Literacy_paragraphe_rate_c <-
  sum(student_g5_b_c$Competency_level_Literacy_paragraphe)/
  (as.integer(length(student_g5_b_c$Sex)))

Grade5_Literacy_story_rate_c <-
  sum(student_g5_b_c$Competency_level_Literacy_story)/
  (as.integer(length(student_g5_b_c$Sex)))

#arrange data frame

Grade_all_Basic_Literacy_Treatment <- c(Grade_all_Literacy_debutant_rate_t*100,
                                        Grade_all_Literacy_lettre_rate_t*100,
                                        Grade_all_Literacy_word_rate_t*100,
                                        Grade_all_Literacy_paragraphe_rate_t*100,
                                        Grade_all_Literacy_story_rate_t*100)

Grade_all_Basic_Literacy_Control <- c(Grade_all_Literacy_debutant_rate_c*100,
                                      Grade_all_Literacy_lettre_rate_c*100,
                                      Grade_all_Literacy_word_rate_c*100,
                                      Grade_all_Literacy_paragraphe_rate_c*100,
                                      Grade_all_Literacy_story_rate_c*100)

Grade3_Basic_Literacy_Treatment <- c(Grade3_Literacy_debutant_rate_t*100,
                                     Grade3_Literacy_lettre_rate_t*100,
                                     Grade3_Literacy_word_rate_t*100,
                                     Grade3_Literacy_paragraphe_rate_t*100,
                                     Grade3_Literacy_story_rate_t*100)

Grade3_Basic_Literacy_Control <- c(Grade3_Literacy_debutant_rate_c*100,
                                   Grade3_Literacy_lettre_rate_c*100,
                                   Grade3_Literacy_word_rate_c*100,
                                   Grade3_Literacy_paragraphe_rate_c*100,
                                   Grade3_Literacy_story_rate_c*100)

Grade4_Basic_Literacy_Treatment <- c(Grade4_Literacy_debutant_rate_t*100,
                                     Grade4_Literacy_lettre_rate_t*100,
                                     Grade4_Literacy_word_rate_t*100,
                                     Grade4_Literacy_paragraphe_rate_t*100,
                                     Grade4_Literacy_story_rate_t*100)

Grade4_Basic_Literacy_Control <- c(Grade4_Literacy_debutant_rate_c*100,
                                   Grade4_Literacy_lettre_rate_c*100,
                                   Grade4_Literacy_word_rate_c*100,
                                   Grade4_Literacy_paragraphe_rate_c*100,
                                   Grade4_Literacy_story_rate_c*100)

Grade5_Basic_Literacy_Treatment <- c(Grade5_Literacy_debutant_rate_t*100,
                                     Grade5_Literacy_lettre_rate_t*100,
                                     Grade5_Literacy_word_rate_t*100,
                                     Grade5_Literacy_paragraphe_rate_t*100,
                                     Grade5_Literacy_story_rate_t*100)

Grade5_Basic_Literacy_Control <- c(Grade5_Literacy_debutant_rate_c*100,
                                   Grade5_Literacy_lettre_rate_c*100,
                                   Grade5_Literacy_word_rate_c*100,
                                   Grade5_Literacy_paragraphe_rate_c*100,
                                   Grade5_Literacy_story_rate_c*100)

Percentage <- c(Grade_all_Basic_Literacy_Treatment,
                Grade_all_Basic_Literacy_Control,
                Grade3_Basic_Literacy_Treatment,
                Grade3_Basic_Literacy_Control,
                Grade4_Basic_Literacy_Treatment,
                Grade4_Basic_Literacy_Control,
                Grade5_Basic_Literacy_Treatment,
                Grade5_Basic_Literacy_Control)

Percentage <- round(Percentage, 1)

Level <- c("0 Beginner", "1 Letter", "2 Word", "3 Paragraph", "4 Story")
Group <- c("Treatment", "Treatment", "Treatment", "Treatment", "Treatment",
           "Control", "Control", "Control", "Control", "Control",
           "G3_Treatment", "G3_Treatment", "G3_Treatment", "G3_Treatment", "G3_Treatment",
           "G3_Control", "G3_Control", "G3_Control", "G3_Control", "G3_Control",
           "G4_Treatment", "G4_Treatment", "G4_Treatment", "G4_Treatment", "G4_Treatment",
           "G4_Control", "G4_Control", "G4_Control", "G4_Control", "G4_Control",
           "G5_Treatment", "G5_Treatment", "G5_Treatment", "G5_Treatment", "G5_Treatment",
           "G5_Control", "G5_Control", "G5_Control", "G5_Control", "G5_Control")

Basic_Literacy <- data.frame(Group,
                             Level,
                             Percentage)

Basic_Literacy$Level <- factor(Basic_Literacy$Level, 
                               levels = c("4 Story", 
                                          "3 Paragraph", 
                                          "2 Word", "1 Letter", "0 Beginner"))

Basic_Literacy <- ddply(Basic_Literacy, .(Group),
                        transform, pos = cumsum(Percentage) - (0.5 * Percentage))

g <- ggplot(Basic_Literacy, aes(x=Group, y=Percentage))
g <- g + geom_bar(stat="identity", aes(fill=Level), width=0.5)
g <- g + scale_x_discrete(limits=c("Treatment", "Control", 
                                   "G3_Treatment", "G3_Control", 
                                   "G4_Treatment", "G4_Control",
                                   "G5_Treatment", "G5_Control"))
g <- g + theme(axis.text = element_text(size = 8))
g <- g + geom_text(aes(y=pos, label = paste0(Percentage, "%")), size = 2.5)  
g <- g + theme(axis.title.x = element_blank())
g <- g + ylab("%") 
g <- g + theme(axis.title.y = element_text(angle = 0, 
                                           colour = "black",size = 9,
                                           vjust = 1)) 
g <- g + scale_y_continuous(breaks=seq(0,100,by=10),limits=c(0,101))

plot(g)

ggsave("graph_literacy.png",
       dpi = 1000, 
       width = 8, height = 4, plot = g)

#

g <- g + scale_fill_manual(
  values=c("0 Beginner"="gray20", "1 Letter"="gray30", 
           "2 Word"="gray40", "3 Paragraph"="gray50", 
           "4 Story"="gray60"), 
  name = "Level")

#

g <- g + ggtitle("Percentage of Pupils on Basic Literacy by Level (Baseline)")
g <- g + theme(plot.title = element_text(hjust = 0.5))
g <- g + theme(plot.title = element_text(size=7))

g <- g + theme(plot.caption=element_text(size=5, hjust=0.5, face="italic", color="black"))
g <- g + theme(legend.text = element_text(size=5))
g <- g + theme(legend.title = element_blank())
g <- g + labs(caption = "N. of Grade 3 (Treatment): 1,895, N. (Control): 1,615
              N. of Grade 4 (Treatment): 1,488, N. (Control): 1,199
              N. of Grade 5 (Treatment): 1,174, N. (Control): 1,086")


#Appendix 1#####

student_b$total_score_math <- c(student_b$Q1_correct_wrong +
                                  student_b$Q2_correct_wrong +
                                  student_b$Q3_correct_wrong +
                                  student_b$Q4_correct_wrong +
                                  student_b$Q5_correct_wrong +
                                  student_b$Q6_correct_wrong +
                                  student_b$Q7_correct_wrong +
                                  student_b$Q8_correct_wrong +
                                  student_b$Q9_correct_wrong +
                                  student_b$Q10_correct_wrong +
                                  student_b$Q11_correct_wrong +
                                  student_b$Q12_correct_wrong +
                                  student_b$Q13_correct_wrong +
                                  student_b$Q14_correct_wrong +
                                  student_b$Q15_correct_wrong +
                                  student_b$Q16_correct_wrong +
                                  student_b$Q17_correct_wrong +
                                  student_b$Q18_correct_wrong +
                                  student_b$Q19_correct_wrong +
                                  student_b$Q20_correct_wrong +
                                  student_b$Q21_correct_wrong +
                                  student_b$Q22_correct_wrong +
                                  student_b$Q23_correct_wrong +
                                  student_b$Q24_correct_wrong +
                                  student_b$Q25_correct_wrong +
                                  student_b$Q26_correct_wrong +
                                  student_b$Q27_correct_wrong +
                                  student_b$Q28_correct_wrong +
                                  student_b$Q29_correct_wrong +
                                  student_b$Q30_correct_wrong +
                                  student_b$Q31_correct_wrong +
                                  student_b$Q32_correct_wrong +
                                  student_b$Q33_correct_wrong +
                                  student_b$Q34_correct_wrong +
                                  student_b$Q35_correct_wrong +
                                  student_b$Q36_correct_wrong +
                                  student_b$Q37_correct_wrong +
                                  student_b$Q38_correct_wrong +
                                  student_b$Q39_correct_wrong +
                                  student_b$Q40_correct_wrong +
                                  student_b$Q41_correct_wrong +
                                  student_b$Q42_correct_wrong +
                                  student_b$Q43_correct_wrong +
                                  student_b$Q44_correct_wrong)

#Figure A-1: all grade

student_all_b <- subset(student_b, Attendance_Numeracy==1&Attendance_Literacy==1)

student_all_b_t <- subset(student_all_b,
                          treatment==1)

student_all_b_c <- subset(student_all_b,
                          treatment==0)

nrow(student_all_b_t)
nrow(student_all_b_c)

student_all_b$treatment_r <- student_all_b$treatment

student_all_b$treatment_r <- replace(student_all_b$treatment_r, 
                                     which(student_all_b$treatment==0), "Control")

student_all_b$treatment_r <- replace(student_all_b$treatment_r, 
                                     which(student_all_b$treatment==1), "Treatment")

cols <- c("Treatment"="red", "Control"="blue") 

g <- ggplot(student_all_b)
g <- g + geom_line(aes(total_score_math, 
                       group=treatment_r, color=treatment_r,
                       linetype=treatment_r), linewidth=0.5, stat=("density"))
g <- g + scale_colour_manual(values = cols)
g <- g + scale_linetype_manual(values=c("Treatment"="solid", 
                                        "Control"="dashed"))
g <- g + xlab("Total Score") 
g <- g + ylab("Density") 
g <- g + scale_x_continuous(breaks=seq(0,44,by=5),limits=c(0,44))
g <- g + theme(axis.title.x = element_text(size = 9)) 
g <- g + theme(axis.title.y = element_text(angle = 0, 
                                           colour = "black",size = 8,
                                           vjust = 1)) 
g <- g + theme(legend.text = element_text(size=9))
g <- g + theme(legend.title = element_blank())
g <- g + theme(legend.position="bottom")
g <- g + theme(legend.key.width = unit(2,"cm"))
plot(g)
ggsave(file = "Total_score_dens_baseline_all.png", plot = g,
       dpi = 1000, width = 6, height = 4)

#Figure A-2: grade 3

student_g3_b <- subset(student_b, Grade==3&Attendance_Numeracy==1&Attendance_Literacy==1)

student_g3_b_t <- subset(student_g3_b,
                         treatment==1)

student_g3_b_c <- subset(student_g3_b,
                         treatment==0)

nrow(student_g3_b_t)
nrow(student_g3_b_c)

student_g3_b$treatment_r <- student_g3_b$treatment

student_g3_b$treatment_r <- replace(student_g3_b$treatment_r, 
                                    which(student_g3_b$treatment==0), "Control")

student_g3_b$treatment_r <- replace(student_g3_b$treatment_r, 
                                    which(student_g3_b$treatment==1), "Treatment")

cols <- c("Treatment"="red", "Control"="blue") 

g <- ggplot(student_g3_b)
g <- g + geom_line(aes(total_score_math, 
                       group=treatment_r, color=treatment_r,
                       linetype=treatment_r), size=0.5, stat=("density"))
g <- g + scale_colour_manual(values = cols)
g <- g + scale_linetype_manual(values=c("Treatment"="solid", 
                                        "Control"="dashed"))
g <- g + xlab("Total Score") 
g <- g + ylab("Density") 
g <- g + scale_x_continuous(breaks=seq(0,44,by=5),limits=c(0,44))
g <- g + theme(axis.title.x = element_text(size = 9)) 
g <- g + theme(axis.title.y = element_text(angle = 0, 
                                           colour = "black",size = 8,
                                           vjust = 1)) 
g <- g + theme(legend.text = element_text(size=9))
g <- g + theme(legend.title = element_blank())
g <- g + theme(legend.position="bottom")
g <- g + theme(legend.key.width = unit(2,"cm"))
plot(g)
ggsave(file = "Total_score_dens_baseline_g3.png", plot = g,
       dpi = 1000, width = 6, height = 4)

#Figure A-3: grade 4

student_g4_b <- subset(student_b, 
                       Grade==4&Attendance_Numeracy==1&Attendance_Literacy==1)

student_g4_b_t <- subset(student_g4_b,
                         treatment==1)

student_g4_b_c <- subset(student_g4_b,
                         treatment==0)

nrow(student_g4_b_t)
nrow(student_g4_b_c)

student_g4_b$treatment_r <- student_g4_b$treatment

student_g4_b$treatment_r <- replace(student_g4_b$treatment_r, 
                                    which(student_g4_b$treatment==0), "Control")

student_g4_b$treatment_r <- replace(student_g4_b$treatment_r, 
                                    which(student_g4_b$treatment==1), "Treatment")

cols <- c("Treatment"="red", "Control"="blue") 

g <- ggplot(student_g4_b)
g <- g + geom_line(aes(total_score_math, 
                       group=treatment_r, color=treatment_r,
                       linetype=treatment_r), size=0.5, stat=("density"))
g <- g + scale_colour_manual(values = cols)
g <- g + scale_linetype_manual(values=c("Treatment"="solid", 
                                        "Control"="dashed"))
g <- g + xlab("Total Score") 
g <- g + ylab("Density") 
g <- g + scale_x_continuous(breaks=seq(0,44,by=5),limits=c(0,44))
g <- g + theme(axis.title.x = element_text(size = 9)) 
g <- g + theme(axis.title.y = element_text(angle = 0, 
                                           colour = "black",size = 8,
                                           vjust = 1)) 
g <- g + theme(legend.text = element_text(size=9))
g <- g + theme(legend.title = element_blank())
g <- g + theme(legend.position="bottom")
g <- g + theme(legend.key.width = unit(2,"cm"))
plot(g)
ggsave(file = "Total_score_dens_baseline_g4.png", plot = g,
       dpi = 1000, width = 6, height = 4)

#Figure A-4: grade 5

student_g5_b <- subset(student_b, Grade==5&Attendance_Numeracy==1&Attendance_Literacy==1)

student_g5_b_t <- subset(student_g5_b,
                         treatment==1)

student_g5_b_c <- subset(student_g5_b,
                         treatment==0)

nrow(student_g5_b_t)
nrow(student_g5_b_c)

student_g5_b$treatment_r <- student_g5_b$treatment

student_g5_b$treatment_r <- replace(student_g5_b$treatment_r, 
                                    which(student_g5_b$treatment==0), "Control")

student_g5_b$treatment_r <- replace(student_g5_b$treatment_r, 
                                    which(student_g5_b$treatment==1), "Treatment")

cols <- c("Treatment"="red", "Control"="blue") 

g <- ggplot(student_g5_b)
g <- g + geom_line(aes(total_score_math, 
                       group=treatment_r, color=treatment_r,
                       linetype=treatment_r), size=0.5, stat=("density"))
g <- g + scale_colour_manual(values = cols)
g <- g + scale_linetype_manual(values=c("Treatment"="solid", 
                                        "Control"="dashed"))
g <- g + xlab("Total Score") 
g <- g + ylab("Density") 
g <- g + scale_x_continuous(breaks=seq(0,44,by=5),limits=c(0,44))
g <- g + theme(axis.title.x = element_text(size = 9)) 
g <- g + theme(axis.title.y = element_text(angle = 0, 
                                           colour = "black",size = 8,
                                           vjust = 1)) 
g <- g + theme(legend.text = element_text(size=9))
g <- g + theme(legend.title = element_blank())
g <- g + theme(legend.position="bottom")
g <- g + theme(legend.key.width = unit(2,"cm"))
plot(g)
ggsave(file = "Total_score_dens_baseline_g5.png", plot = g,
       dpi = 1000, width = 6, height = 4)

#Appendix 2#####

#estimate IRT scores: baseline

student_b <- subset(student_b,
                    Attendance_Numeracy==1)

test_data_baseline <- cbind.data.frame(student_b$Q1_correct_wrong,
                                       student_b$Q2_correct_wrong,
                                       student_b$Q3_correct_wrong,
                                       student_b$Q4_correct_wrong,
                                       student_b$Q5_correct_wrong,
                                       student_b$Q6_correct_wrong,
                                       student_b$Q7_correct_wrong,
                                       student_b$Q8_correct_wrong,
                                       student_b$Q9_correct_wrong,
                                       student_b$Q10_correct_wrong,
                                       student_b$Q11_correct_wrong,
                                       student_b$Q12_correct_wrong,
                                       student_b$Q13_correct_wrong,
                                       student_b$Q14_correct_wrong,
                                       student_b$Q15_correct_wrong,
                                       student_b$Q16_correct_wrong,
                                       student_b$Q17_correct_wrong,
                                       student_b$Q18_correct_wrong,
                                       student_b$Q19_correct_wrong,
                                       student_b$Q20_correct_wrong,
                                       student_b$Q21_correct_wrong,
                                       student_b$Q22_correct_wrong,
                                       student_b$Q23_correct_wrong,
                                       student_b$Q24_correct_wrong,
                                       student_b$Q25_correct_wrong,
                                       student_b$Q26_correct_wrong,
                                       student_b$Q27_correct_wrong,
                                       student_b$Q28_correct_wrong,
                                       student_b$Q29_correct_wrong,
                                       student_b$Q30_correct_wrong,
                                       student_b$Q31_correct_wrong,
                                       student_b$Q32_correct_wrong,
                                       student_b$Q33_correct_wrong,
                                       student_b$Q34_correct_wrong,
                                       student_b$Q35_correct_wrong,
                                       student_b$Q36_correct_wrong,
                                       student_b$Q37_correct_wrong,
                                       student_b$Q38_correct_wrong,
                                       student_b$Q39_correct_wrong,
                                       student_b$Q40_correct_wrong,
                                       student_b$Q41_correct_wrong,
                                       student_b$Q42_correct_wrong,
                                       student_b$Q43_correct_wrong,
                                       student_b$Q44_correct_wrong)

# estimate parameters in Table A-1 in Appendix 2
para.2PL_baseline <-est(resp=test_data_baseline,model="2PL",engine="ltm")

# draw test information curve: Figure B-3 
I <- tif(ip=para.2PL_baseline$est)

# draw tif curve

# add SE 
I_data <- cbind.data.frame(I$x,I$f)
names(I_data) <- c("x","f")
I_data$SE_I <- 1/sqrt(I_data$f) 

# draw tif

par(oma = c(0, 1, 0, 3))

plot(I_data$x,
     I_data$f,
     xlab='math scores (irt)', 
     ylab='information',
     main="",
     type='l',
     lwd=1.2,
     xlim=c(-2.3,2.6),
     xaxp=c(-2,2,4))

# add se curve
par(new = T)

plot(I_data$x, I_data$SE_I,
     type='l', 
     lty='dashed',
     axes = FALSE,
     xlab="",
     ylab="",
     xlim=c(-2.3,2.6))

axis(4)

mtext("standard error",
      side = 4,
      line = 3)

# estimate student ability in numbers and the basic four operations
theta.est_base <-mlebme(resp=test_data_baseline,
                        ip=para.2PL_baseline$est,
                        method="BM")

student_b$irt_score_base <- theta.est_base[,1]

# density curves of the irt scores in baseline survey
# Figure B-1

colnames(student_b)

student_b$treatment_r <- student_b$treatment

student_b$treatment_r <- replace(student_b$treatment_r, 
                                 which(student_b$treatment==0), "Control")

student_b$treatment_r <- replace(student_b$treatment_r, 
                                 which(student_b$treatment==1), "Treatment")

cols <- c("Treatment"="red", "Control"="blue") 

#

g <- ggplot(student_b)
g <- g + geom_line(aes(irt_score_base, 
                       group=treatment_r, color=treatment_r,
                       linetype=treatment_r), linewidth=0.5, stat=("density"))
g <- g + scale_colour_manual(values = cols)
g <- g + scale_linetype_manual(values=c("Treatment"="solid", 
                                        "Control"="dashed"))
g <- g + xlab("Math Score (irt)") 
g <- g + ylab("Density") 
g <- g + theme(axis.title.x = element_text(size = 9)) 
g <- g + theme(axis.title.y = element_text(angle = 0, 
                                           colour = "black",size = 8,
                                           vjust = 1)) 
g <- g + theme(legend.text = element_text(size=9))
g <- g + theme(legend.title = element_blank())
g <- g + theme(legend.position="bottom")
g <- g + theme(legend.key.width = unit(2,"cm"))
plot(g)
ggsave(file = "irt_score_dens_baseline_all.png", plot = g,
       dpi = 1000, width = 6, height = 4)

###end-line_math#
#estimate IRT scores

student_e <-subset(student_e,
                   Attendance_Numeracy_endline==1)

test_data_endline <- cbind.data.frame(student_e$Q1_correct_wrong,
                                      student_e$Q2_correct_wrong,
                                      student_e$Q3_correct_wrong,
                                      student_e$Q4_correct_wrong,
                                      student_e$Q5_correct_wrong,
                                      student_e$Q6_correct_wrong,
                                      student_e$Q7_correct_wrong,
                                      student_e$Q8_correct_wrong,
                                      student_e$Q9_correct_wrong,
                                      student_e$Q10_correct_wrong,
                                      student_e$Q11_correct_wrong,
                                      student_e$Q12_correct_wrong,
                                      student_e$Q13_correct_wrong,
                                      student_e$Q14_correct_wrong,
                                      student_e$Q15_correct_wrong,
                                      student_e$Q16_correct_wrong,
                                      student_e$Q17_correct_wrong,
                                      student_e$Q18_correct_wrong,
                                      student_e$Q19_correct_wrong,
                                      student_e$Q20_correct_wrong,
                                      student_e$Q21_correct_wrong,
                                      student_e$Q22_correct_wrong,
                                      student_e$Q23_correct_wrong,
                                      student_e$Q24_correct_wrong,
                                      student_e$Q25_correct_wrong,
                                      student_e$Q26_correct_wrong,
                                      student_e$Q27_correct_wrong,
                                      student_e$Q28_correct_wrong,
                                      student_e$Q29_correct_wrong,
                                      student_e$Q30_correct_wrong,
                                      student_e$Q31_correct_wrong,
                                      student_e$Q32_correct_wrong,
                                      student_e$Q33_correct_wrong,
                                      student_e$Q34_correct_wrong,
                                      student_e$Q35_correct_wrong,
                                      student_e$Q36_correct_wrong,
                                      student_e$Q37_correct_wrong,
                                      student_e$Q38_correct_wrong,
                                      student_e$Q39_correct_wrong,
                                      student_e$Q40_correct_wrong,
                                      student_e$Q41_correct_wrong,
                                      student_e$Q42_correct_wrong,
                                      student_e$Q43_correct_wrong,
                                      student_e$Q44_correct_wrong)

# estimate parameters in Table A-2 in Appendix 2
para.2PL_endline <-est(resp=test_data_endline,model="2PL",engine="ltm")

# draw test information curve: Figure B-4
# draw tif curve

# add SE 
I_data <- cbind.data.frame(I$x,I$f)
names(I_data) <- c("x","f")
I_data$SE_I <- 1/sqrt(I_data$f) 

# draw tif

par(oma = c(0, 1, 0, 3))

plot(I_data$x,
     I_data$f,
     xlab='math scores (irt)', 
     ylab='information',
     main="",
     type='l',
     lwd=1.2,
     xlim=c(-3.4, 1.8),
     xaxp=c(-3,1,4))

# add se curve
par(new = T)

plot(I_data$x, I_data$SE_I,
     type='l', 
     lty='dashed',
     axes = FALSE,
     xlab="",
     ylab="",
     xlim=c(-3.4, 1.8),
     ylim=c(0, 2.5))

axis(4)

mtext("standard error",
      side = 4,
      line = 3)

# estimate student ability in numbers and the basic four operations
theta.est_end <-mlebme(resp=test_data_endline,
                       ip=para.2PL_endline$est,
                       method="BM")

student_e$irt_score_end <- theta.est_end[,1]

# Figure B-2
# density curves of the irt scores in end-line survey

student_e$treatment_r <- student_e$treatment

student_e$treatment_r <- replace(student_e$treatment_r, 
                                 which(student_e$treatment==0), "Control")

student_e$treatment_r <- replace(student_e$treatment_r, 
                                 which(student_e$treatment==1), "Treatment")

cols <- c("Treatment"="red", "Control"="blue") 

#

g <- ggplot(student_e)
g <- g + geom_line(aes(irt_score_end, 
                       group=treatment_r, color=treatment_r,
                       linetype=treatment_r), linewidth=0.5, stat=("density"))
g <- g + scale_colour_manual(values = cols)
g <- g + scale_linetype_manual(values=c("Treatment"="solid", 
                                        "Control"="dashed"))
g <- g + xlab("Math Score (irt)") 
g <- g + ylab("Density") 
g <- g + theme(axis.title.x = element_text(size = 9)) 
g <- g + theme(axis.title.y = element_text(angle = 0, 
                                           colour = "black",size = 8,
                                           vjust = 1)) 
g <- g + theme(legend.text = element_text(size=9))
g <- g + theme(legend.title = element_blank())
g <- g + theme(legend.position="bottom")
g <- g + theme(legend.key.width = unit(2,"cm"))
plot(g)
ggsave(file = "irt_score_dens_endline_all.png", plot = g,
       dpi = 1000, width = 6, height = 4)
