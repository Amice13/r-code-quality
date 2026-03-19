#===============================================================================
#  File:     Fig5.R
#  Date:     20 May 2023
#  Paper:    Online Disinformation predicts Inaccurate Beliefs about Election 
#            Fairness among both Winners and Losers 
#  Journal:  Comparative Political Studies
#  Authors:  Marlene Mauk & Max Grömping
#  Purpose:  Produce Figure 5
#===============================================================================

rm(list=ls())
options(scipen=999)
options(java.parameters = "-Xmx4g")

# SESSION INFO ------------------------------------------------------------
sessionInfo()   # R version 4.2.3 (2023-03-15 ucrt)
                # Platform: x86_64-w64-mingw32/x64 (64-bit)
                # Running under: Windows 10 x64 (build 19044)

# LOAD PACKAGES ---------------------------------------------------------------
if (!require("pacman")) {
  install.packages("pacman")
  library("pacman")
} else {
  library("pacman")
}
pacman::p_load(ggplot2)
pacman::p_load(reshape2)
pacman::p_load(ggrepel)
pacman::p_load(tidyr)
pacman::p_load(dplyr)
pacman::p_load(readxl)

# SET WORKING DIRECTORY ---------------------------------------------------
#setwd("--- your directory ---")


# HELPER FUNCTIONS --------------------------------------------------------
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}


# LOAD DATA ---------------------------------------------------------------

# robustness checks
rob <- read_xlsx("robustness_checks.xlsx")



# FIGURE 5 ----------------------------------------------------------------
rob <- rob %>%
  rename("Category" = "...1",
         "Type" = "...2",
         "Elect Integrity\n(M1) Estimate" = `electoral integrity (Model 1)`,
         "Elect Integrity\n(M1) Lower CI" = `...4`,
         "Elect Integrity\n(M1) Upper CI" = `...5`,
         "Winner\n(M1) Estimate" = `winner (Model 1)`,
         "Winner\n(M1) Lower CI" = `...7`,
         "Winner\n(M1) Upper CI" = `...8`,
         "Elect Integrity X\nDisinfo\n(M2) Estimate" = `electoral integrity x disinformation (Model 2)`,
         "Elect Integrity X\nDisinfo\n(M2) Lower CI" = `...10`,
         "Elect Integrity X\nDisinfo\n(M2) Upper CI" = `...11`,
         "Winner X\nDisinfo\n(M3) Estimate" = `winner x disinformation (Model 3)`,
         "Winner X\nDisinfo\n(M3) Lower CI" = `...13`,
         "Winner X\nDisinfo\n(M3) Upper CI" = `...14`,         
         "Elect Integrity X\nWinner X\nDisinfo\n(M4) Estimate" = `electoral integrity x disinformation x winner (Model 4)`,
         "Elect Integrity X\nWinner X\nDisinfo\n(M4) Lower CI" = `...16`,
         "Elect Integrity X\nWinner X\nDisinfo\n(M4) Upper CI" = `...17`,         
         )

rob <- rob %>%
  slice(-1)

rob <- rob %>%
  mutate(Category = ifelse(row_number() == 1, "Main", Category),
         Type = ifelse(row_number() == 1, "Main", Type))

rob <- rob %>%
  mutate(Category = case_when(
    Category == "alternative DV" ~ "Alt DV",
    Category == "alternative IV" ~ "Alt IV",
    Category == "model specification" ~ "Model Spec",
    Category == "sample" ~ "Sample",
    TRUE ~ Category
  )) %>%
  fill(Category)

rob <- rob %>%
  mutate(Type = case_when(
    Type == "clustered SEs at country level" ~ "Clustered SEs at country lvl",
    Type == "DV: pef1" ~ '"Votes counted fairly"',
    Type == "DV: pef2" ~ '"Opposition (not) prevented"',
    Type == "DV: pef6" ~ '"Election officials fair"',
    Type == "DV: pef7" ~ '"Rich people (not) buy elections"',
    Type == "DV: pef8" ~ '"(Not) violence at polls"',
    Type == "expert-level PEI" ~ "Expert-level PEI",
    Type == "foreign disinfo" ~ "Foreign Disinfo",
    Type == "government disinfo" ~ "Gov't Disinfo",
    Type == "partisan disinfo" ~ "Partisan Disinfo",
    Type == "without control variables" ~ "W/o control vars",
    Type == "without countries that held elections" ~ "W/o elections during fieldwork",
    Type == "without countries with low internet penetration" ~ "W/o countries w low internet",
    Type == "without election monitors" ~ "W/o monitors",
    Type == "without winner/loser" ~ "W/o winner",
    TRUE ~ Type
  ))

rob <- as.data.frame(rob)

rob$Effect <- NA

# Append 72 empty rows to rob
rob <- rob %>%
  bind_rows(data.frame(matrix(ncol = ncol(rob), nrow = 72), stringsAsFactors = FALSE))
rob <- rob[,-c(19:36), ]

# Change values in rob$Type
rob <- rob %>%
  mutate(Effect = case_when(
    row_number() <= 18 ~ "Elect Integrity\\n(M1)",
    row_number() <= 36 ~ "Winner\\n(M1)",
    row_number() <= 54 ~ "Elect Integrity X\\nDisinfo\\n(M2)",
    row_number() <= 72 ~ "Winner X\\nDisinfo\\n(M3)",
    TRUE ~ "Elect Integrity X\\nWinner X\\nDisinfo\\n(M4)"
  ))

# pivot manually
rob[19:36, 3:5] <- rob[1:18, 6:8]
rob[37:54, 3:5] <- rob[1:18, 9:11]
rob[55:72, 3:5] <- rob[1:18, 12:14]
rob[73:90, 3:5] <- rob[1:18, 15:17]
rob[19:36, 1:2] <- rob[1:18, 1:2]
rob[37:54, 1:2] <- rob[1:18, 1:2]
rob[55:72, 1:2] <- rob[1:18, 1:2]
rob[73:90, 1:2] <- rob[1:18, 1:2]


colnames(rob)[3:5] <- c("estimate", "lower_ci", "upper_ci") # Rename columns 3-5
rob <- rob[, -c(6:17)] # Remove columns 6-17

rob <- rob %>%
  mutate_at(vars(3:5), ~as.numeric(gsub(",", "", .)))

rob$Effect <- gsub("\\\\n", "\n",  rob$Effect)

rob$Effect <- factor(rob$Effect, levels = c("Elect Integrity\n(M1)",
                                            "Winner\n(M1)",
                                            "Elect Integrity X\nDisinfo\n(M2)",
                                            "Winner X\nDisinfo\n(M3)",
                                            "Elect Integrity X\nWinner X\nDisinfo\n(M4)"))

rob$Type <- factor(rob$Type, levels = c("Main",
                                        "\"Votes counted fairly\"" ,
                                        "\"Opposition (not) prevented\"",
                                        "\"Election officials fair\"" ,
                                        "\"Rich people (not) buy elections\"",
                                        "\"(Not) violence at polls\"",
                                        "Expert-level PEI",
                                        "NELDA",
                                        "V-Dem Clean Elections",
                                        "Gov't Disinfo",
                                        "Foreign Disinfo",
                                        "Partisan Disinfo",
                                        "W/o winner",
                                        "W/o monitors",
                                        "W/o control vars",
                                        "Clustered SEs at country lvl",
                                        "W/o elections during fieldwork",
                                        "W/o countries w low internet"))

rob$Category <- factor(rob$Category, levels = c("Main",
                                                "Alt DV" ,
                                                "Alt IV",
                                                "Model Spec",
                                                "Sample"))

ggplot(rob, aes(x = Type, y = estimate)) + 
  geom_pointrange(aes(group = factor(Category),ymin = lower_ci, ymax = upper_ci)) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  scale_x_discrete(limits=rev) +
  coord_flip() +
  facet_grid(Category~Effect ,scales = "free", space = "free_y") +
  theme_classic() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=0.5),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 10)) +
  labs(x = "", y = "Estimate", title = "Estimated effect on perceptions of election fairness of…")

ggsave(filename = "figures/Fig5.jpg", width = 7.4, height = 6.5, dpi = 600, units = "in",  device = "jpg")









