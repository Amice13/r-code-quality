# Load Packages

library(tidyverse)
library(readxl)
library(basicspace)

#######################################
# Combine with Ballot Paper Data
#######################################

# Load all ballot box results

box_data_2015 <- read_excel("wahlbezirksdaten_2015.xlsx")
box_data_2020 <- read_excel("wahlbezirksdaten_2020.xlsx")

# Select only relevant variables

box_data <- bind_rows(box_data_2015, 
                         box_data_2020) %>%
  select(candidate_id, votes, briefwahl)

# Aggregate Data

box_data <-  box_data %>%
  group_by(candidate_id, briefwahl) %>%
  summarise(votes = sum(votes, na.rm = TRUE)) %>%
  ungroup()

# Load Ballot Data

ballot_data_2015 <- read_excel("ballot_data_2015.xlsx")
ballot_data_2020 <- read_excel("ballot_data_2020.xlsx")

ballot_data <- bind_rows(ballot_data_2015,
                         ballot_data_2020)

# Create Var with Number of candidates

ballot_data <- ballot_data %>%
  group_by(wahljahr, wknummer, partei) %>%
  mutate(nr_candidates = n()) %>%
  ungroup()

# Join Ballot Paper Data

df <- left_join(box_data, 
                ballot_data, 
                by = "candidate_id") %>%
  arrange(wahljahr, wknummer, partei, listenplatz, briefwahl)

# Compare party results between postal and election day voters (not in paper)

rename_parties <- function(x){
  
  y <- x
  y[x == "GRÜNE"] <- "Greens"
  y[x == "AFD"] <- "AfD"
  y[x == "LINKE"] <- "Left Party"
  
  factor(y, levels = c("AfD", "CDU", "FDP", "Greens", "Left Party", "SPD"))
}

df %>%
  group_by(wahljahr, briefwahl) %>%
  mutate(total_votes = sum(votes, na.rm = TRUE)) %>%
  group_by(wahljahr, partei, briefwahl) %>%
  summarise(total_party_votes = sum(votes, na.rm = TRUE),
            total_votes = mean(total_votes, na.rm = TRUE)) %>%
  mutate(vote_share_party = total_party_votes/total_votes) %>%
  arrange(wahljahr, partei, briefwahl) %>%
  mutate(Context = ifelse(briefwahl == 1, 
                          "Postal Voting", "Election Day Voting")) %>%
  ungroup() %>%
  mutate(party = rename_parties(partei)) -> party_success_boxes

ggplot(party_success_boxes, aes(x = party,
                                y = vote_share_party, 
                                fill = Context)) +
  geom_col(position = position_dodge(width = 0.9),
           color = "black") +
  facet_wrap(~ wahljahr) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom") +
  ylab("Party Vote Share") +
  xlab("Party") +
  scale_fill_manual(values = c("grey35", "grey75"))

# Compute Vote Share

df <- df %>%
  group_by(wahljahr, wknummer, partei, briefwahl) %>%
  mutate(party_votes = sum(votes, na.rm = TRUE)) %>%
  mutate(vote_share = votes/party_votes) %>%
  ungroup()

# Wide Data

df$briefwahl_indicator <- ifelse(df$briefwahl, "vs_post", "vs_box")

data_wide <- reshape2::dcast(select(df, 
                          candidate_id,
                          briefwahl_indicator,
                          vote_share),
                          candidate_id ~ briefwahl_indicator, 
                          value.var = "vote_share")

data_wide$vs_difference <- data_wide$vs_post - data_wide$vs_box

nrow(data_wide)

# Plot Difference between Post and Box Voting

ggplot(data_wide, aes(x = vs_difference)) +
  geom_histogram(bins = 75, color = "white") +
  theme_bw(base_size = 14) +
  xlab("Vote Share Postal - Vote Share Ballot Box") +
  ylab("Count")

ggsave(file = "vote_share_diff_post_box.pdf", 
       width = 10, 
       height = 6)

# Merge Ballot Paper Data

data_wide <- left_join(data_wide, 
                       ballot_data, 
                       by = "candidate_id")

# Prepare data for Regression

data_wide$age <- data_wide$wahljahr - data_wide$jahrgang

data_wide$lplatz <- factor(data_wide$listenplatz, 
                           levels = 1:10)

# Write data to Stata

haven::write_dta(data_wide,
                 "post_box_data_wide.dta",
                 version = 14)





