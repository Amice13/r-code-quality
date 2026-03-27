# This file contains replication code for the R Journal submission titled: "A more efficient approach to converting ASCII files and cleaning data in R with the speedycode package" by Jacob Harris

#### Loading in essential packages ####
remove(list = ls())
library(speedycode)
library(readroper)
library(dplyr)
library(labelled)

#### Replication code for the section: Reading in Data with the readroper Package ####

card5 <- read_rpr(
  col_positions = c(18,21,23:66),
  widths = rep(1,46),
  col_names = c("Dayof_week", "Q1", "Q2", "Q3a", "Q3b", "Q4a1", "Q4a2", 
                "Q4b", "Q5", "Q6a", "Q6b", "Q7", "Q8", "Q9", "Q10", "Q11a", 
                "Q11b", "Q11c", "Q11d", "Q11e", "Q12", "Q13", "Q14", "Q15", 
                "Q16", "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23a", 
                "Q23b", "Q23c", "Q23d", "Q24", "Q25", "Q26", "Q27", "Q28", 
                "Q29", "Q30a", "Q30b", "Q30c", "Q30d", "Q30e"),
  filepath = "file-path-to-ascii-file.dat",
  card_read = 5,
  cards = 5
)
head(card5)

#### Replication code for the section: Generating New Variable Names with speedy_varnames ####

speedy_varnames("Q", 1, 50)

#### Replication code for the section: Debugging Errors with debug_ascii ####

# Correct lengths
debug_ascii(col_positions = c(18,21,23:66),
            widths = rep(1,46),
            col_names = c("Dayof_week", "Q1", "Q2", "Q3a", "Q3b", "Q4a1", "Q4a2",
                          "Q4b", "Q5", "Q6a", "Q6b", "Q7", "Q8", "Q9", "Q10", 
                          "Q11a", "Q11b", "Q11c", "Q11d", "Q11e", "Q12", "Q13", 
                          "Q14", "Q15",  "Q16", "Q17", "Q18", "Q19", "Q20", "Q21", 
                          "Q22", "Q23a",  "Q23b", "Q23c", "Q23d", "Q24", "Q25", 
                          "Q26", "Q27", "Q28",  "Q29", "Q30a", "Q30b", "Q30c", 
                          "Q30d", "Q30e"))

# Incorrect lengths (see missing Q30e variable name)
debug_ascii(col_positions = c(18,21,23:66),
            widths = rep(1,46),
            col_names = c("Dayof_week", "Q1", "Q2", "Q3a", "Q3b", "Q4a1", "Q4a2",
                          "Q4b", "Q5", "Q6a", "Q6b", "Q7", "Q8", "Q9", "Q10", 
                          "Q11a", "Q11b", "Q11c", "Q11d", "Q11e", "Q12", "Q13", 
                          "Q14", "Q15",  "Q16", "Q17", "Q18", "Q19", "Q20", "Q21", 
                          "Q22", "Q23a",  "Q23b", "Q23c", "Q23d", "Q24", "Q25", 
                          "Q26", "Q27", "Q28",  "Q29", "Q30a", "Q30b", "Q30c", 
                          "Q30d"))

#### Replication code for the section: Labelling Variables and Values with speedy_labels ####

# For all Card 5 variables: 
speedy_labels(card5, nrows = 5)

# As demonstrated in the paper
card5 <- card5 %>% 
  mutate_all(as.numeric) %>% 
  select(1:5)

speedy_labels(card5, nrows = 5)


#### Replication code for the section: Final steps ####
all_cards <- read_dta("file-path-to-ascii-file-converted.dta")
library(remotes)
remotes::install_github("y2analytics/y2clerk")
library(y2clerk)

all_cards %>% freqs() %>% 
  filter(variable != "Weight",
         variable != "Age") %>% 
  select(-stat)

#### Replication code for the section: Renaming variables with speedy_rename ####
library(readr)
anes <- read_csv("file-path-to-example-anes-data.csv")
speedy_rename(anes)

#### Replication code for the section: Changing variable classes with speedy_classes ####
speedy_classes(anes)

#### Replication code for the section: Adding labels with speedy_labels ####
speedy_labels(anes, nrows = 5)	








