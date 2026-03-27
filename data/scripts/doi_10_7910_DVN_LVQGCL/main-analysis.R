# This is the file crating the main analysis in the paper (figure 2, table 7)

# clear workspace
rm(list=ls(all=TRUE))

# load required packages
library(rio)
library(tidyverse)
library(list)
library(xtable)

# load data

df <- read.csv("data/main-data.csv", stringsAsFactors = FALSE)

# recode DK responses to NA
df$Income[df$Income == 5] <- NA
#df$Education[df$Education == 7] <- NA
df$Education[df$Education == 8] <- 3 # Lumps "Other", with "low education"
df$Children[df$Children == 6] <- NA
df$Gender[df$Gender == 3] <- NA
df$PartyMember[df$PartyMember == 3] <- NA
df$GovProcure <- ifelse(df$CommissionSurvey == 1,1,0) #create GovProcure dummy
# simple authoritarian personality index 
df <- df %>%
  mutate(aut_1 = ifelse(ObeyParents == 3 | ObeyParents == 4, 1, 0),
         aut_2 = ifelse(TeacherAuthority == 3 | TeacherAuthority == 4, 1, 0),
         aut_3 = ifelse(FamilyIndividual == 3 | FamilyIndividual == 4, 1, 0),
         AutPersonality = aut_1 + aut_2 + aut_3)


#Create treatment status indicators
df$y_confidence <- ifelse(is.na(df$ConfidenceTreat),df$ConfidenceControl,df$ConfidenceTreat)
df$treat_confidence <- ifelse(df$Group == 1, 1, 0)  #treatment indicator

df$y_system <- ifelse(is.na(df$SystemTreat),df$SystemControl,df$SystemTreat) #single list variable
df$treat_system <- ifelse(df$Group == 2, 1, 0)  #treatment indicator

df$y_corruption <- ifelse(is.na(df$CorruptionTreat),df$CorruptionControl,df$CorruptionTreat)
df$treat_corruption <- ifelse(df$Group == 2, 1, 0)  #treatment indicator

df$y_censor <- ifelse(is.na(df$CensorTreat),df$CensorControl,df$CensorTreat)
df$treat_censor <- ifelse(df$Group == 1, 1, 0)  #treatment indicator


# in the main analysis we recode Direct Questions to binary (DK coded as NA)
df$ConfidenceDirect[df$ConfidenceDirect == 3] <- NA
df$ConfidenceDirect[df$ConfidenceDirect == 2] <- 0

df$SystemDirect[df$SystemDirect == 3] <- NA
df$SystemDirect[df$SystemDirect == 2] <- 0

df$CorruptionDirect[df$CorruptionDirect == 3] <- NA
df$CorruptionDirect[df$CorruptionDirect == 2] <- 0

df$CensorDirect[df$CensorDirect == 3] <- NA
df$CensorDirect[df$CensorDirect == 2] <- 0

# run the falsification estimates
source("code/main-estimates.R")

#matrix to store point estimates and standard errors for direct and indirect methods
#direct estimates stored in columns 1,2, indirect in columns 3,4
  estimates <- matrix(NA, nrow = 4, ncol = 4)

  #calculations, fill matrix
  #rows are confidence, system, corruption, censor
  #columns are direct proportion, direct se, indirect proportion, indirect se
  estimates[1,1] <- mean(df$ConfidenceDirect, na.rm = TRUE)
  estimates[1,2] <- sqrt(var(df$ConfidenceDirect, na.rm=TRUE) / (length(which(!is.na(df$ConfidenceDirect))-1)))
  estimates[1,3] <- mean(df$ConfidenceTreat, na.rm = TRUE) - mean(df$ConfidenceControl, na.rm = TRUE)
  estimates[1,4] <- sqrt((var(df$ConfidenceTreat, na.rm=TRUE) / length(which(!is.na(df$ConfidenceTreat))) + (var(df$ConfidenceControl, na.rm = TRUE) / (length(which(!is.na(df$ConfidenceControl)))))))

  estimates[2,1] <- mean(df$CorruptionDirect, na.rm = TRUE)
  estimates[2,2] <- sqrt(var(df$CorruptionDirect, na.rm=TRUE) / (length(which(!is.na(df$CorruptionDirect))-1)))
  estimates[2,3] <- mean(df$CorruptionTreat, na.rm = TRUE) - mean(df$CorruptionControl, na.rm = TRUE)
  estimates[2,4] <- sqrt((var(df$CorruptionTreat, na.rm=TRUE) / length(which(!is.na(df$CorruptionTreat))) + (var(df$CorruptionControl, na.rm = TRUE) / (length(which(!is.na(df$CorruptionControl)))))))

  estimates[3,1] <- mean(df$SystemDirect, na.rm = TRUE)
  estimates[3,2] <- sqrt(var(df$SystemDirect, na.rm=TRUE) / (length(which(!is.na(df$SystemDirect))-1)))
  estimates[3,3] <- mean(df$SystemTreat, na.rm = TRUE) - mean(df$SystemControl, na.rm = TRUE)
  estimates[3,4] <- sqrt((var(df$SystemTreat, na.rm=TRUE) / length(which(!is.na(df$SystemTreat))) + (var(df$SystemControl, na.rm = TRUE) / (length(which(!is.na(df$SystemControl)))))))
  
  estimates[4,1] <- mean(df$CensorDirect, na.rm = TRUE)
  estimates[4,2] <- sqrt(var(df$CensorDirect, na.rm=TRUE) / (length(which(!is.na(df$CensorDirect))-1)))
  estimates[4,3] <- mean(df$CensorTreat, na.rm = TRUE) - mean(df$CensorControl, na.rm = TRUE)
  estimates[4,4] <- sqrt((var(df$CensorTreat, na.rm = TRUE) / length(which(!is.na(df$CensorTreat))) + (var(df$CensorControl, na.rm = TRUE) / (length(which(!is.na(df$CensorControl)))))))



  ###Create results table
  ###
  results <- matrix(NA, nrow=12, ncol=3) #create matrix to store proportions and differences
  colnames(results) <- c("Direct", "Indirect", "Difference") #name the columns
  results[1,] <- round(c(estimates[1,1], estimates[1,3], estimates[1,3] - estimates[1,1]), digits = 3) #confidence
  results[4,] <- round(c(estimates[2,1], estimates[2,3], estimates[2,3] - estimates[2,1]), digits = 3) #system
  results[7,] <- round(c(estimates[3,1], estimates[3,3], estimates[3,3] - estimates[3,1]), digits = 3) #corruption
  results[10,] <- round(c(estimates[4,1], estimates[4,3], estimates[4,3] - estimates[4,1]), digits = 3) #censor

  #this adds the standard errors to the matrix
  results[2,] <- round(c(estimates[1,2], estimates[1,4], avg.pred.social.desirability.confidence[[2]][3]), digits = 3) #confidence
  results[5,] <- round(c(estimates[2,2], estimates[2,4], avg.pred.social.desirability.corruption[[2]][3]), digits = 3) #system
  results[8,] <- round(c(estimates[3,2], estimates[3,4], avg.pred.social.desirability.system[[2]][3]), digits = 3) #corruption
  results[11,] <- round(c(estimates[4,2], estimates[4,4], avg.pred.social.desirability.censor[[2]][3]), digits = 3) #censor


  #this adds -n to the matrix, skips the last column
  results[3,] <- round(c(length(which(!is.na(df$ConfidenceDirect))), length(which(!is.na(df$ConfidenceTreat))) + length(which(!is.na(df$ConfidenceControl))),NA), digits = 0) #confidence
  results[6,] <- c(length(which(!is.na(df$CorruptionDirect))), length(which(!is.na(df$CorruptionTreat))) + length(which(!is.na(df$CorruptionControl))),NA) #corruption
  results[9,] <- round(c(length(which(!is.na(df$SystemDirect))), length(which(!is.na(df$SystemTreat))) + length(which(!is.na(df$SystemControl))),NA), digits = 0) #system
  results[12,] <- c(length(which(!is.na(df$CensorDirect))), length(which(!is.na(df$CensorTreat))) + length(which(!is.na(df$CensorControl))),NA) #censor

  Item <- c("Confidence", " " ," ", "Corruption", " "," ", "System", " "," ", "Censorship", " "," ") #create vector of item names for first column of table

  exp_results <- cbind.data.frame(Item,results) #bind vector of item names with matrix of estimates
  names(exp_results)[c(1)] <- " "  #rename first column (vector of names) to blank for presentation purposes

  exp_results[2,2] <- paste0("(", format(unlist(exp_results[2,2])),")")  ##This formats the standard error cells with parentheses
  exp_results[2,3] <- paste0("(", format(unlist(exp_results[2,3])),")")
  exp_results[2,4] <- paste0("(", format(unlist(exp_results[2,4])),")")
  exp_results[5,2] <- paste0("(", format(unlist(exp_results[5,2])),")")
  exp_results[5,3] <- paste0("(", format(unlist(exp_results[5,3])),")")
  exp_results[5,4] <- paste0("(", format(unlist(exp_results[5,4])),")")
  exp_results[8,2] <- paste0("(", format(unlist(exp_results[8,2])),")")
  exp_results[8,3] <- paste0("(", format(unlist(exp_results[8,3])),")")
  exp_results[8,4] <- paste0("(", format(unlist(exp_results[8,4])),")")
  exp_results[11,2] <- paste0("(", format(unlist(exp_results[11,2])),")")
  exp_results[11,3] <- paste0("(", format(unlist(exp_results[11,3])),")")
  exp_results[11,4] <- paste0("(", format(unlist(exp_results[11,4])),")")

  exp_results[3,2] <- paste0("n = ", format(unlist(exp_results[3,2])),"")  ##This formats the -n cells
  exp_results[3,3] <- paste0("n = ", format(unlist(exp_results[3,3])),"")
  exp_results[6,2] <- paste0("n = ", format(unlist(exp_results[6,2])),"")
  exp_results[6,3] <- paste0("n = ", format(unlist(exp_results[6,3])),"")
  exp_results[9,2] <- paste0("n = ", format(unlist(exp_results[9,2])),"")
  exp_results[9,3] <- paste0("n = ", format(unlist(exp_results[9,3])),"")
  exp_results[12,2] <- paste0("n = ", format(unlist(exp_results[12,2])),"")
  exp_results[12,3] <- paste0("n = ", format(unlist(exp_results[12,3])),"")

  exp_results[1,4] <- paste0("", format(unlist(exp_results[1,4])),"***")  ##This adds stars for significance (based on hand calculations)
  exp_results[4,4] <- paste0("", format(unlist(exp_results[4,4])),"***")
  exp_results[7,4] <- paste0("", format(unlist(exp_results[7,4])),"***")
  exp_results[10,4] <- paste0("", format(unlist(exp_results[10,4])),"***")


  #Export table to latex using xtable
  comm <- paste0("\\hline \n \\multicolumn{4}{l}",
                 "{\\scriptsize{Standard errors in parentheses. }} \n")  #Comment "Standard errors in parentheses"

  print(xtable(exp_results, align = c("l", "l", "c", "c", "c")), type = "latex", file = "output/results_table2.tex", floating = F, include.rownames = F,
        hline.after=c(-1,0,3,6,9), add.to.row = list(pos = list(12), command = comm)) #export the table


  ###Calculate confidence intervals around indirect estimates
  ###Forest Plot figure
  label <- c("Confidence Direct", "Confidence Indirect", "Corruption Direct", "Corruption Indirect", "System Direct", "System Indirect", "Censorship Direct", "Censorship Indirect")
  mean <- c(estimates[1,1], estimates[1,3], estimates[2,1], estimates[2,3], estimates[3,1], estimates[3,3], estimates[4,1], estimates[4,3])
  lower <- c(estimates[1,1] - 1.96*estimates[1,2], estimates[1,3] - 1.96*estimates[1,4],
             estimates[2,1] - 1.96*estimates[2,2], estimates[2,3] - 1.96*estimates[2,4],
             estimates[3,1] - 1.96*estimates[3,2], estimates[3,3] - 1.96*estimates[3,4],
             estimates[4,1] - 1.96*estimates[4,2], estimates[4,3] - 1.96*estimates[4,4])
  upper <- c(estimates[1,1] + 1.96*estimates[1,2], estimates[1,3] + 1.96*estimates[1,4],
             estimates[2,1] + 1.96*estimates[2,2], estimates[2,3] + 1.96*estimates[2,4],
             estimates[3,1] + 1.96*estimates[3,2], estimates[3,3] + 1.96*estimates[3,4],
             estimates[4,1] + 1.96*estimates[4,2], estimates[4,3] + 1.96*estimates[4,4])

  df <- data.frame(label, mean, lower, upper) #create a dataframe with above vectors
  df$label <- factor(df$label, levels=(df$label)) #label dataframe
  df

  df$item <- c("Confidence", "Confidence", "Corruption", "Corruption", "System", "System", "Censorship", "Censorship")

  df$support <- c(rep("Specific support", 4), rep("Diffuse support", 4))

  df$support <- factor(df$support, levels = c("Specific support", "Diffuse support"))

  df$order <- c(1,2,3,4,5,6,7,8)

  df$label <- factor(df$label, levels=rev(df$label))

  df$label <- factor(df$label, levels=rev(c("Confidence Direct", "Confidence Indirect", "Corruption Direct", "Corruption Indirect",
                                            "System Direct", "System Indirect", "Censorship Direct", "Censorship Indirect")))

  ylabel <- rep(c("Indirect", "Direct"), 4)

  #create the plot
ggplot(data = df, aes(y = label, x = mean, xmin = lower, xmax = upper, linetype = item, shape = item, color = item)) +
      geom_point(size=3) +
      geom_errorbarh(height = .01) +
      ylab("Questioning technique") + xlab("Est. proportion of supporters (95% CI)") +
      scale_y_discrete(label = ylabel) +
      theme_light(base_size = 14) +
      #theme(axis.text.x = element_text(angle = 90, size = 14), panel.background = element_rect(fill = "white", colour = "grey"), panel.grid.minor = element_line("light grey"), axis.title = element_text(size = 14)) +
      scale_x_continuous(limits=c(0.3,1), breaks=c(.3, .4, .5, .6, .7, .8, .9, 1)) +
      scale_linetype_manual(breaks = c("Confidence", "Corruption", "System", "Censorship"),
                            values = c("Censorship" = "longdash", "Confidence" = "solid", "Corruption" = "F1", "System" = "dashed"),
                            name = "Item:") +
      scale_shape_manual(breaks = c("Confidence", "Corruption", "System", "Censorship"),
                         values = c("Censorship" = 4, "Confidence" = 16, "Corruption" = 15, "System" = 17),
                         name = "Item:") +
      scale_color_manual(breaks = c("Confidence", "Corruption", "System", "Censorship"),
                         values = c("Censorship" = "#984ea3", "Confidence" = "#e41a1c", "Corruption" = "#4daf4a", "System" = "#377eb8"),
                         name = "Item:") +
      theme(legend.position = 'top',
            panel.grid.major.y = element_blank()) +
      facet_grid(support ~ ., scales = "free", space = "free") +
      guides(color=guide_legend(nrow=2,byrow=TRUE), 
             shape=guide_legend(nrow=2,byrow=TRUE),
             linetype=guide_legend(nrow=2,byrow=TRUE))


  ggsave("output/fp_main.pdf", width = 5, height = 5, units = "in")
  
  
  ##################################
  # Following code produce the main estimates including DKs in the analysis (table 8)
  ##################################
  
# load data 
  df <- read.csv("data/main-data.csv", stringsAsFactors = FALSE)
  
  # recode DK responses to NA
  df$Income[df$Income == 5] <- NA
  df$Education[df$Education == 8] <- 3 # Lumps "Other", with "low education"
  df$Children[df$Children == 6] <- NA
  df$Gender[df$Gender == 3] <- NA
  df$PartyMember[df$PartyMember == 3] <- NA
  df$GovProcure <- ifelse(df$CommissionSurvey == 1,1,0) #create GovProcure dummy
  # simple authoritarian personality index 
  df <- df %>%
    mutate(aut_1 = ifelse(ObeyParents == 3 | ObeyParents == 4, 1, 0),
           aut_2 = ifelse(TeacherAuthority == 3 | TeacherAuthority == 4, 1, 0),
           aut_3 = ifelse(FamilyIndividual == 3 | FamilyIndividual == 4, 1, 0),
           AutPersonality = aut_1 + aut_2 + aut_3)
  
  
  #Create treatment status indicators
  df$y_confidence <- ifelse(is.na(df$ConfidenceTreat),df$ConfidenceControl,df$ConfidenceTreat)
  df$treat_confidence <- ifelse(df$Group == 1, 1, 0)  #treatment indicator
  
  df$y_system <- ifelse(is.na(df$SystemTreat),df$SystemControl,df$SystemTreat) #single list variable
  df$treat_system <- ifelse(df$Group == 2, 1, 0)  #treatment indicator
  
  df$y_corruption <- ifelse(is.na(df$CorruptionTreat),df$CorruptionControl,df$CorruptionTreat)
  df$treat_corruption <- ifelse(df$Group == 2, 1, 0)  #treatment indicator
  
  df$y_censor <- ifelse(is.na(df$CensorTreat),df$CensorControl,df$CensorTreat)
  df$treat_censor <- ifelse(df$Group == 1, 1, 0)  #treatment indicator
  
  
  # this time recode Direct Questions to binary bit with DK as 0
  df$ConfidenceDirect[df$ConfidenceDirect == 3] <- 0
  df$ConfidenceDirect[df$ConfidenceDirect == 2] <- 0
  
  df$SystemDirect[df$SystemDirect == 3] <- 0
  df$SystemDirect[df$SystemDirect == 2] <- 0
  
  df$CorruptionDirect[df$CorruptionDirect == 3] <- 0
  df$CorruptionDirect[df$CorruptionDirect == 2] <- 0
  
  df$CensorDirect[df$CensorDirect == 3] <- 0
  df$CensorDirect[df$CensorDirect == 2] <- 0
  
  # run the falsification estimates
  source("code/main-estimates.R")
  
  #matrix to store point estimates and standard errors for direct and indirect methods
  #direct estimates stored in columns 1,2, indirect in columns 3,4
  estimates <- matrix(NA, nrow = 4, ncol = 4)
  
  #calculations, fill matrix
  #rows are confidence, system, corruption, censor
  #columns are direct proportion, direct se, indirect proportion, indirect se
  estimates[1,1] <- mean(df$ConfidenceDirect, na.rm = TRUE)
  estimates[1,2] <- sqrt(var(df$ConfidenceDirect, na.rm=TRUE) / (length(which(!is.na(df$ConfidenceDirect))-1)))
  estimates[1,3] <- mean(df$ConfidenceTreat, na.rm = TRUE) - mean(df$ConfidenceControl, na.rm = TRUE)
  estimates[1,4] <- sqrt((var(df$ConfidenceTreat, na.rm=TRUE) / length(which(!is.na(df$ConfidenceTreat))) + (var(df$ConfidenceControl, na.rm = TRUE) / (length(which(!is.na(df$ConfidenceControl)))))))
  
  estimates[2,1] <- mean(df$CorruptionDirect, na.rm = TRUE)
  estimates[2,2] <- sqrt(var(df$CorruptionDirect, na.rm=TRUE) / (length(which(!is.na(df$CorruptionDirect))-1)))
  estimates[2,3] <- mean(df$CorruptionTreat, na.rm = TRUE) - mean(df$CorruptionControl, na.rm = TRUE)
  estimates[2,4] <- sqrt((var(df$CorruptionTreat, na.rm=TRUE) / length(which(!is.na(df$CorruptionTreat))) + (var(df$CorruptionControl, na.rm = TRUE) / (length(which(!is.na(df$CorruptionControl)))))))
  
  estimates[3,1] <- mean(df$SystemDirect, na.rm = TRUE)
  estimates[3,2] <- sqrt(var(df$SystemDirect, na.rm=TRUE) / (length(which(!is.na(df$SystemDirect))-1)))
  estimates[3,3] <- mean(df$SystemTreat, na.rm = TRUE) - mean(df$SystemControl, na.rm = TRUE)
  estimates[3,4] <- sqrt((var(df$SystemTreat, na.rm=TRUE) / length(which(!is.na(df$SystemTreat))) + (var(df$SystemControl, na.rm = TRUE) / (length(which(!is.na(df$SystemControl)))))))
  
  estimates[4,1] <- mean(df$CensorDirect, na.rm = TRUE)
  estimates[4,2] <- sqrt(var(df$CensorDirect, na.rm=TRUE) / (length(which(!is.na(df$CensorDirect))-1)))
  estimates[4,3] <- mean(df$CensorTreat, na.rm = TRUE) - mean(df$CensorControl, na.rm = TRUE)
  estimates[4,4] <- sqrt((var(df$CensorTreat, na.rm = TRUE) / length(which(!is.na(df$CensorTreat))) + (var(df$CensorControl, na.rm = TRUE) / (length(which(!is.na(df$CensorControl)))))))
  
  
  
  ###Create results table
  ###
  results <- matrix(NA, nrow=12, ncol=3) #create matrix to store proportions and differences
  colnames(results) <- c("Direct", "Indirect", "Difference") #name the columns
  results[1,] <- round(c(estimates[1,1], estimates[1,3], estimates[1,3] - estimates[1,1]), digits = 3) #confidence
  results[4,] <- round(c(estimates[2,1], estimates[2,3], estimates[2,3] - estimates[2,1]), digits = 3) #system
  results[7,] <- round(c(estimates[3,1], estimates[3,3], estimates[3,3] - estimates[3,1]), digits = 3) #corruption
  results[10,] <- round(c(estimates[4,1], estimates[4,3], estimates[4,3] - estimates[4,1]), digits = 3) #censor
  
  #this adds the standard errors to the matrix
  results[2,] <- round(c(estimates[1,2], estimates[1,4], avg.pred.social.desirability.confidence[[2]][3]), digits = 3) #confidence
  results[5,] <- round(c(estimates[2,2], estimates[2,4], avg.pred.social.desirability.corruption[[2]][3]), digits = 3) #system
  results[8,] <- round(c(estimates[3,2], estimates[3,4], avg.pred.social.desirability.system[[2]][3]), digits = 3) #corruption
  results[11,] <- round(c(estimates[4,2], estimates[4,4], avg.pred.social.desirability.censor[[2]][3]), digits = 3) #censor
  
  
  #this adds -n to the matrix, skips the last column
  results[3,] <- round(c(length(which(!is.na(df$ConfidenceDirect))), length(which(!is.na(df$ConfidenceTreat))) + length(which(!is.na(df$ConfidenceControl))),NA), digits = 0) #confidence
  results[6,] <- c(length(which(!is.na(df$CorruptionDirect))), length(which(!is.na(df$CorruptionTreat))) + length(which(!is.na(df$CorruptionControl))),NA) #corruption
  results[9,] <- round(c(length(which(!is.na(df$SystemDirect))), length(which(!is.na(df$SystemTreat))) + length(which(!is.na(df$SystemControl))),NA), digits = 0) #system
  results[12,] <- c(length(which(!is.na(df$CensorDirect))), length(which(!is.na(df$CensorTreat))) + length(which(!is.na(df$CensorControl))),NA) #censor
  
  Item <- c("Confidence", " " ," ", "Corruption", " "," ", "System", " "," ", "Censorship", " "," ") #create vector of item names for first column of table
  
  exp_results <- cbind.data.frame(Item,results) #bind vector of item names with matrix of estimates
  names(exp_results)[c(1)] <- " "  #rename first column (vector of names) to blank for presentation purposes
  
  exp_results[2,2] <- paste0("(", format(unlist(exp_results[2,2])),")")  ##This formats the standard error cells with parentheses
  exp_results[2,3] <- paste0("(", format(unlist(exp_results[2,3])),")")
  exp_results[2,4] <- paste0("(", format(unlist(exp_results[2,4])),")")
  exp_results[5,2] <- paste0("(", format(unlist(exp_results[5,2])),")")
  exp_results[5,3] <- paste0("(", format(unlist(exp_results[5,3])),")")
  exp_results[5,4] <- paste0("(", format(unlist(exp_results[5,4])),")")
  exp_results[8,2] <- paste0("(", format(unlist(exp_results[8,2])),")")
  exp_results[8,3] <- paste0("(", format(unlist(exp_results[8,3])),")")
  exp_results[8,4] <- paste0("(", format(unlist(exp_results[8,4])),")")
  exp_results[11,2] <- paste0("(", format(unlist(exp_results[11,2])),")")
  exp_results[11,3] <- paste0("(", format(unlist(exp_results[11,3])),")")
  exp_results[11,4] <- paste0("(", format(unlist(exp_results[11,4])),")")
  
  exp_results[3,2] <- paste0("n = ", format(unlist(exp_results[3,2])),"")  ##This formats the -n cells
  exp_results[3,3] <- paste0("n = ", format(unlist(exp_results[3,3])),"")
  exp_results[6,2] <- paste0("n = ", format(unlist(exp_results[6,2])),"")
  exp_results[6,3] <- paste0("n = ", format(unlist(exp_results[6,3])),"")
  exp_results[9,2] <- paste0("n = ", format(unlist(exp_results[9,2])),"")
  exp_results[9,3] <- paste0("n = ", format(unlist(exp_results[9,3])),"")
  exp_results[12,2] <- paste0("n = ", format(unlist(exp_results[12,2])),"")
  exp_results[12,3] <- paste0("n = ", format(unlist(exp_results[12,3])),"")
  
  exp_results[1,4] <- paste0("", format(unlist(exp_results[1,4])),"***")  ##This adds stars for significance (based on hand calculations)
  exp_results[4,4] <- paste0("", format(unlist(exp_results[4,4])),"***")
  exp_results[7,4] <- paste0("", format(unlist(exp_results[7,4])),"***")
  exp_results[10,4] <- paste0("", format(unlist(exp_results[10,4])),"***")
  
  
  #Export table to latex using xtable
  comm <- paste0("\\hline \n \\multicolumn{4}{l}",
                 "{\\scriptsize{Standard errors in parentheses. }} \n")  #Comment "Standard errors in parentheses"
  
  print(xtable(exp_results, align = c("l", "l", "c", "c", "c")), type = "latex", file = "output/results_table.tex", floating = F, include.rownames = F,
        hline.after=c(-1,0,3,6,9), add.to.row = list(pos = list(12), command = comm)) #export the table
  
  
