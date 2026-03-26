####################################################################
####################################################################
## Replication Material
##
## Michael Jankowski and Stefan M?ller
##
## michael.jankowski@uol.de | stefan.mueller@ucd.ie
## 
## Incumbency Advantage in Lower-Order PR Elections:
## Evidence from the Irish Context, 1942-2019
## 
## Electoral Studies

## File: 03_descriptive_plots.R

## See 00_description_data_and_scripts.pdf for a detailed 
## overview of the required data and the outputs of this file.

####################################################################
####################################################################

# load required packages

library(dplyr)   # CRAN v1.0.4
library(tidyr)   # CRAN v1.1.2
library(ggplot2) # CRAN v3.3.3
library(Hmisc)   # CRAN v4.4-2
library(scales)  # CRAN v1.1.1
library(forcats) # CRAN v0.5.1


# custom ggplot2 theme
theme_baser <- function (){
  theme_minimal()  %+replace%
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.border = element_rect(fill=NA,color="black", size=0.5,
                                      linetype="solid"),
          legend.title = element_text(size = 15),
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold",
                                    margin=margin(0,0,5,0)),
          legend.position = "bottom",
          axis.ticks = element_line(size = 0.3),
          axis.ticks.length = unit(0.2,"cm"),
          legend.text=element_text(size = 13),
          strip.text.y = element_text(size = 16, hjust = 0.5, face = "bold",
                                      margin=margin(5,5,5,5), angle = 270),
          strip.text.x = element_text(size = 16, hjust = 0.5, face = "bold",
                                      margin=margin(0,0,5,0)),
          axis.text = element_text(colour="black", size = 13),
          axis.title = element_text(size = 13, hjust = 0.5))
}

theme_set(theme_baser())

# load data of all local election results
complete <- readRDS(file = "data_local_elections_complete.rds")


## Figure 1 ----

complete_councils <- complete %>% 
  filter(elected == 1) %>% 
  group_by(election_year, council_name, const_name) %>% 
  count() %>% 
  rename(n_elected_const = n) %>% 
  group_by(election_year, council_name) %>% 
  mutate(n_elected_countil = sum(n_elected_const)) %>% 
  mutate(const_name_elected = paste0(const_name, " (", n_elected_const, ")")) %>% 
  mutate(council_name_elected = paste0(council_name, " (", n_elected_countil, ")")) 


complete_councils_sum <- complete_councils %>% 
  group_by(election_year, council_name, n_elected_countil, council_name_elected) %>% 
  summarise(constituencies = paste(const_name_elected, collapse = "; ")) %>% 
  mutate(dual_mandate_dummy = ifelse(election_year > 2000, "No Dual Mandate", "Dual Mandate"))


ggplot(complete_councils_sum, aes(x = factor(election_year), y = fct_rev(council_name))) +
  geom_point(size = 3.5) +
  labs(x = "Year of local election", y = "Council") 
ggsave("fig_01.eps",
       width = 10, height = 8)
ggsave("fig_01.pdf",
       width = 10, height = 8)

## Figure A01 ----

# get distribution of seats per list and candidates per list

candidates_elected_const <- complete %>% 
  filter(elected == 1) %>% 
  group_by(election_year, council_name, const_name) %>% 
  count() %>% 
  mutate(type = "Candidates elected\nin constituency")

candidates_running_const <- complete %>% 
  group_by(election_year, council_name, const_name) %>% 
  count() %>% 
  mutate(type = "Candidates running\nin constituency")


candidates_sum <- bind_rows(candidates_elected_const, 
                            candidates_running_const)

candidates_sum %>% 
  group_by(type) %>% 
  summarise(min = min(n),
            max = max(n),
            median = median(n),
            mean = mean(n))


candidates_sum_stats <- candidates_sum %>% 
  group_by(election_year, type) %>% 
  summarise(min = min(n),
            max = max(n),
            mean = mean(n)) %>% 
  mutate(label = paste0("Min: ", min, "; Max: ", max, "; Mean: ", round(mean, 1)))


candidates_sum$type <- fct_rev(candidates_sum$type)

candidates_sum_stats$type <- fct_rev(candidates_sum_stats$type)


ggplot(candidates_sum, aes(x = n, fill = type,
                           colour = type)) +
  geom_bar() +
  scale_x_continuous(limits = c(0, 40), breaks = c(seq(0, 40, 4))) +
  facet_grid(election_year~type) +
  labs(x = "Number of candidates", y = "Count") +
  geom_text(data = candidates_sum_stats,
            aes(label = label, x = 15, y = 40),
            hjust = 0) +
  scale_fill_manual(values = c("grey50", "black")) +
  scale_colour_manual(values = c("grey60", "grey20")) +
  theme(legend.position = "none")
ggsave("fig_a01.pdf",
       width = 8, height = 12)


## Figure A02 ----

# levels of turnout in local and general elections

dat_turnout <- read.csv("data_turnout_ireland.csv") %>% 
  filter(year >= 1960) # filter only elections since 1960 because records for previous local elections are incomplete


# summary statistics for paper
dat_turnout %>% 
  group_by(type_election) %>% 
  summarise(mean = mean(turnout, na.rm = TRUE),
            min = min(turnout))

ggplot(dat_turnout, aes(x = year, y = turnout /100,
                        colour = type_election,
                        shape = type_election)) +
  geom_point(size = 4) +
  geom_line() +
  scale_shape_manual(values = c(16, 15)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  scale_x_continuous(breaks = c(seq(1940, 2020, 10))) +
  scale_colour_manual(values = c("grey70", "black")) +
  labs(x = "Year", y = "Voter turnout") +
  theme(legend.title = element_blank())
ggsave("fig_a02.pdf", width = 8, height = 4)


## Figure A03 ----


dat_ines <- readRDS("data_ines_subset.rds")

ines_candidate_called <- dat_ines %>% 
  filter(ines %in% c(2002, 2004, 2007)) %>% 
  filter(!is.na(candidate_called_home)) %>% 
  mutate(candidate_called_home = ifelse(candidate_called_home == "yes", 1, 0)) %>% 
  mutate(ines = ifelse(ines == 2002, "2002: General election",
                       ifelse(ines == 2004, "2004: Local/European elections",
                              ifelse(ines == 2007, "2007: General elections", NA))))


dat_sum_candidate_called <- ines_candidate_called %>%
  group_by(ines) %>%
  summarise(mean = mean(candidate_called_home),
            se = sd(candidate_called_home) / sqrt(n()),
            ci_lower_95 = mean - 1.96 * se,
            ci_upper_95 = mean + 1.96 * se,
            ci_lower_90 = mean - 1.645 * se,
            ci_upper_90 = mean + 1.645 * se)

summary(ines_candidate_called$candidate_called_home)

# check whether confidence intervals are the same
# when treating the variable as a proportion
dat_sum_candidate_called_prop <- ines_candidate_called %>%
  group_by(ines) %>%
  count(candidate_called_home) %>%
  summarise(prop = n / sum(n), 
            lower = lapply(n, prop.test, n = sum(n)), 
            upper = sapply(lower, function(x) x$conf.int[2]), 
            lower = sapply(lower, function(x) x$conf.int[1]))



head(dat_sum_candidate_called_prop)

head(dat_sum_candidate_called)

# yes, the lower and upper 95% confidence intervals are basically identical

ggplot(data = dat_sum_candidate_called,
       aes(x = factor(ines),
           y = mean)) +
  geom_point(size = 2.5) +
  geom_linerange(aes(ymin = ci_lower_95, ymax = ci_upper_95)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  geom_text(aes(label = paste0(100 * round(mean,2), "%")),
            nudge_x = 0.3, vjust = 0.5) +
  labs(x = NULL,
       y = "At least one candidate called to respondent's home")
ggsave("fig_a03.pdf", 
       width = 8, height = 3)


## Figure A04 ----

# calculate the percentage of elected candidates, 
# separately for TDs and non-TDs for each election

complete_elected_boot <- complete %>% 
  group_by(election_year, td_status) %>% 
  do(data.frame(rbind(Hmisc::smean.cl.boot(.$elected)))) 


complete_elected <- complete %>% 
  group_by(election_year, td_status) %>% 
  count() %>% 
  left_join(complete_elected_boot,
            by = c("election_year", "td_status"))


# summary statistics for paper
complete_elected %>% 
  group_by(td_status) %>% 
  summarise(mean = mean(Mean),
            min = min(Mean),
            max = max(Mean))

ggplot(complete_elected, aes(x = factor(election_year), y = Mean,
                             ymin = Lower, ymax = Upper,
                             colour = td_status,
                             shape = td_status)) +
  geom_pointrange(size = 0.8) +
  geom_vline(aes(xintercept = 11.5), linetype = "dashed") +
  annotate("text", x = 10, y = 0.1, label = "Dual mandate\npossible",
           size = 4, colour = "grey30") +
  annotate("text", x = 13, y = 0.1, label = "Dual mandate\nabolished",
           colour = "grey30") +
  annotate("segment", x = 11, xend = 9, 
           y = 0.2, yend = 0.2, colour = "grey30", 
           size = 0.5, 
           arrow = arrow(angle = 25, length = unit(0.3, "cm"))) +
  annotate("segment", x = 12, xend = 14, y = 0.2, yend = 0.2, 
           colour = "grey30", 
           size = 0.5, 
           arrow = arrow(angle = 25, length = unit(0.3, "cm"))) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
  scale_colour_manual(values = c("grey50", "black")) +
  scale_shape_manual(values = c(16, 15)) +
  labs(x = "Year of local election", y = "Percentage of elected candidates\n(and 95% CIs)") +
  theme(legend.title = element_blank())
ggsave("fig_a04.pdf",
       width = 8, height = 4)



## Figure A05 ----


# check how many TDs ran in a constituency 
# and create a binary indicator that shows whether or not a TD was present on the list
complete_td_presence <- complete %>% 
  group_by(election_year, council_name, const_name) %>% 
  summarise(tds_present = sum(is_td)) %>% 
  mutate(td_present_dummy = ifelse(tds_present > 0, 1, 0))

# estimate the proportion with TDs on a list
# and get bootstrapped 95% confidence intervals to account 
# for different sample sizes and missing election data
set.seed(134)
complete_td_presence_boot <- complete_td_presence %>% 
  group_by(election_year) %>% 
  do(data.frame(rbind(Hmisc::smean.cl.boot(.$td_present_dummy)))) %>% 
  mutate(dual_mandate_dummy = ifelse(election_year > 2000, FALSE, TRUE))

# summary statistics for paper
complete_td_presence_boot %>% 
  ungroup() %>% 
  filter(election_year < 1999) %>% 
  summarise(min = min(Mean),
            max = max(Mean))


ggplot(complete_td_presence_boot, aes(x = factor(election_year), 
                                      y = Mean, 
                                      colour = dual_mandate_dummy,
                                      fill = dual_mandate_dummy,
                                      ymin = Lower,
                                      ymax = Upper)) +
  geom_pointrange(size = 0.8) +
  geom_vline(aes(xintercept = 11.5), linetype = "dashed") +
  annotate("text", x = 10, y = 0.1, label = "Dual mandate\npossible",
           size = 4, colour = "grey30") +
  annotate("text", x = 13, y = 0.1, label = "Dual mandate\nabolished",
           colour = "grey30") +
  annotate("segment", x = 11, xend = 9, 
           y = 0.2, yend = 0.2, colour = "grey30", 
           size = 0.5, 
           arrow = arrow(angle = 25, length = unit(0.3, "cm"))) +
  annotate("segment", x = 12, xend = 14, y = 0.2, yend = 0.2, 
           colour = "grey30", 
           size = 0.5, 
           arrow = arrow(angle = 25, length = unit(0.3, "cm"))) +
  #scale_x_continuous(breaks = c(seq(1935, 2020, 5))) +
  scale_colour_manual(values = c("grey50", "black")) +
  labs(x = "Year", y = "Constituencies with TD\n(and 95% CIs)") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "none")
ggsave("fig_a05.pdf", 
       width = 8, height = 3.5)


## Figure A06 ----

# check the percentage of elected TDs
dat_tds <- complete %>% 
  filter(td_status == "TD")

# check that party is the same (candidate in local election and TD)
table(dat_tds$party_same)

dat_tds_prop_elected <- complete %>% 
  filter(td_status == "TD") %>% 
  group_by(election_year) %>% 
  summarise(prop_elected = mean(elected),
            n_tds = n()) 

# aggregate by election year
dat_presence_count <- complete_td_presence %>% 
  group_by(election_year, tds_present) %>% 
  count() %>% 
  filter(election_year < 2000) %>% 
  mutate(td_dummy = ifelse(tds_present == 0, FALSE, TRUE)) %>% 
  group_by(election_year) %>% 
  mutate(perc_present = 100 * n / sum(n))


ggplot(dat_presence_count,
       aes(x = n, y = fct_rev(factor(tds_present)),
           fill = td_dummy)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(perc_present, 0), "%")),
            hjust = 0, nudge_x = 2) +
  facet_wrap(~election_year) +
  scale_x_continuous(limits = c(0, 160)) +
  scale_fill_manual(values = c("grey70", "black")) +
  labs(x = "Number of constituencies",
       y = "TDs on list") +
  theme(legend.position = "none")
ggsave("fig_a06.pdf", width = 8, height = 5)




## Figure A07 ----

# check TD in vote margin (close winner or loser)

dat_td_all <- complete %>% 
  group_by(election_year, elected) %>% 
  summarise(td = sum(is_td)) %>% 
  spread(elected, td) %>%
  mutate(tds_total = as.integer(`0` + `1`)) %>% 
  mutate(tds_not_elected = as.integer(`0`),
         tds_elected = as.integer(`1`)) %>% 
  select(-c(`0`, `1`))

dat_td_all_plot <- dat_td_all %>% 
  filter(election_year < 2003) %>% 
  mutate(prop_elected = tds_elected / tds_total) %>% 
  mutate(label = paste0("TDs running: ", tds_total, "; elected: ",
                        tds_elected))


# get proportions across all elections
dat_td_all_plot %>% 
  ungroup() %>% 
  summarise(min = min(prop_elected),
            avg = mean(prop_elected),
            max = max(prop_elected))

ggplot(dat_td_all_plot, aes(x = prop_elected,
                            y = fct_rev(factor(election_year)))) +
  geom_bar(stat = "identity", colour = "white",
           fill = "grey70") +
  geom_text(aes(label = label),
            hjust = 0, x = 0.05,
            colour = "grey20") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = c(seq(0, 1, 0.2))) +
  labs(x = "Percentage of TDs who got elected", 
       y = "Year of local election")
ggsave("fig_a07.pdf", 
       width = 8, height = 4)



## Figure A08 ----

# only filter candidates who are in vote margin and potentially are included in the RDD analysis
complete_margin <- filter(complete, !is.na(vote_margin))

dat_td_margin <- complete_margin %>%
  filter(election_year < 2002) %>% 
  group_by(election_year) %>% 
  summarise(sum_tds_margin = sum(is_td)) %>% 
  full_join(dat_td_all_plot)

# check how many TDs elected
dat_td_margin_td <- complete_margin %>%
  filter(election_year < 2002) %>% 
  filter(is_td == 1) %>% 
  group_by(election_year) %>% 
  summarise(prop_td_in_margin_elected = mean(elected)) %>% 
  full_join(dat_td_margin) %>% 
  mutate(label = paste0("TDs in vote margin: ", sum_tds_margin))


dat_td_margin_td %>% 
  ungroup() %>% 
  summarise(mean = mean(prop_td_in_margin_elected, na.rm = TRUE))

ggplot(dat_td_margin_td, aes(x = prop_td_in_margin_elected,
                             y = fct_rev(factor(election_year)))) +
  geom_bar(stat = "identity", colour = "white",
           fill = "grey70") +
  geom_text(aes(label = label),
            hjust = 0, x = 0.05,
            colour = "grey20") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = c(seq(0, 1, 0.2))) +
  labs(x = "Percentage of TDs in vote margin who got elected", 
       y = "Year of local election")
ggsave("fig_a08.pdf", 
       width = 8, height = 4)


## Figure A09 ----

# check proportion of elected tds in vote margin

# load dataset with availability of first counts
complete_firstcount <- readRDS("data_local_elections_firstcount.rds")

# count the number of candidate-level observations 
# based on available data on the first count (not the last count which is used in the RDD analysis)
complete_firstcount_count <- complete_firstcount %>% 
  group_by(election_year, elected, td_status) %>% 
  count() %>% 
  spread(elected, n) %>% 
  rename(elected = `1`) %>% 
  mutate(not_elected = ifelse(is.na(`0`), 0, `0`)) %>% 
  mutate(sum_candidates = not_elected + elected) %>% 
  mutate(percent_elected = 100 * elected / sum_candidates) %>% 
  mutate(type = "First count available")


# repeat aggregation for last counts
complete_count <- complete %>% 
  group_by(election_year, elected, td_status) %>% 
  count() %>%
  spread(elected, n) %>% 
  rename(elected = `1`) %>% 
  mutate(not_elected = ifelse(is.na(`0`), 0, `0`)) %>% 
  mutate(sum_candidates = not_elected + elected) %>% 
  mutate(percent_elected = 100 * elected / sum_candidates) %>% 
  mutate(type = "Last count available")

complete_firstcount_count$election_year <- as.character(complete_firstcount_count$election_year)
complete_count$election_year <- as.character(complete_count$election_year)

# bind both dataframes
complete_join <- bind_rows(complete_count, complete_firstcount_count)

ggplot(complete_join, aes(x = sum_candidates, y = fct_rev(election_year),
                          fill = type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sum_candidates), nudge_x = 30,
            colour = "grey40",
            hjust = 0) +
  scale_fill_manual(values = c("grey60", "black")) +
  facet_grid(type~td_status) +
  scale_x_continuous(limits = c(0, 2400)) +
  labs(x = "Number of candidates", y = "Year of local election") +
  theme(legend.position = "none")
ggsave("fig_a09.pdf",
       width = 8, height = 6)



## Figure A10 ----

complete_tds_elected <- complete %>% 
  filter(td_on_list_elected == T) %>% 
  filter(is_td == T) %>% 
  select(election_year, council_name, const_name, party)

complete_marginal_winners_with_td <- complete %>% 
  filter(td_on_list_elected == T) %>% 
  filter(last_winner == TRUE) %>% 
  select(election_year, council_name, const_name,
         last_winner, party_candidate = party)


complete_tds_winners_merged <- left_join(complete_marginal_winners_with_td,
                                         complete_tds_elected)


complete_tds_winners_merged_crosstab <- complete_tds_winners_merged %>% 
  group_by(party, party_candidate) %>% 
  count() %>% 
  mutate(same = ifelse(party == party_candidate, TRUE, FALSE))

ggplot(complete_tds_winners_merged_crosstab, 
       aes(x = party_candidate, y = party, colour = same)) +
  geom_point(aes(size = n)) +
  geom_text(aes(label = n), nudge_y = 0.3) +
  scale_colour_manual(values = c("darkred", "darkgreen")) +
  labs(x = "Party of marginal winner in election t", 
       y = "Party of TD(s) competing in constitutnecy in election t") +
  theme(legend.position = "none")
ggsave("fig_a10.pdf", width = 8, height = 7)



## Figure A11 ----

complete_tds_winners_merged_sum <- complete_tds_winners_merged %>% 
  mutate(party_same = ifelse(party == party_candidate, "Elected TD(s) and marginally elected candidate: same party", 
                             "Elected TD(s) and marginally elected candidate: different parties")) %>% 
  group_by(election_year, council_name, const_name) %>% 
  mutate(n_tds = n()) %>% 
  group_by(n_tds, party_same) %>% 
  count() 

ggplot(complete_tds_winners_merged_sum, aes(x = factor(n_tds),
                                            y = party_same,
                                            colour = party_same)) +
  geom_point(aes(size = n)) +
  geom_text(aes(label = n), nudge_y = 0.3) +
  labs(x = "Elected TDs in constituency", y = NULL) +
  scale_colour_manual(values = c("darkred", "darkgreen")) +
  theme(legend.position = "none")
ggsave("fig_a11.pdf", width = 8, height = 3)


## Figure A12 ----

## Figure A12 is a screenshot from a website and does not require any code
