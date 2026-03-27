####################################################################
####################################################################
## Replication Material
##
## Shaun Bowler, Gail McElroy, Stefan Müller:
## Voter Expectations of Government Formation in Coalition Systems: 
## The Importance of the Information Context.
## European Journal of Political Research

## File: 01c_get_coalitions_since_1990.R

## See 000_description_replication_material.pdf for a detailed 
## overview of the required data and the outputs of this file.

####################################################################
####################################################################

## get all coalitions on the Land level between 1990 and 2017 
## and count their frequencies

## load packages

library(dplyr) # A Grammar of Data Manipulation, CRAN v1.0.2
library(stringr) # Simple, Consistent Wrappers for Common String Operations, CRAN v1.4.0
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics, CRAN v3.3.2


## load ggplot2 scheme
source("function_theme_base.R")

## load dataset with state election results between 1990 and 2017
dat_since1990 <- read.csv("laender_election_results_1990-2017.csv")

min(dat_since1990$date)

max(dat_since1990$date)

## 106 
nrow(dat_since1990)

dat_since1990_govparties <- dat_since1990 %>% 
    group_by(government_parties) %>% 
    count() %>% 
    rename(coalitions_formed19902017_abs = n) %>% 
    mutate(government_parties = str_to_lower(government_parties)) %>% 
    mutate(government_parties = str_replace_all(government_parties, "linke", "left")) %>% 
    mutate(government_parties = str_replace_all(government_parties, "pds", "left")) %>% 
    mutate(government_parties = str_replace_all(government_parties, "gruenen", "greens")) %>% 
    mutate(government_parties = str_replace_all(government_parties, ", ", "_"))


## change the order of parties of some of the existing coalitions
## to merge them with the full dataset (which always has the same order of parties
## and does not who consider which party is the largest one)

harmonise_governments <- c("
                           'left_spd_greens'='spd_left_greens';
                           'cdu_greens_fdp'='cdu_fdp_greens';
                           'csu'='cdu';
                           'csu_fdp'='cdu_fdp';
                           'spd_cdu'='cdu_spd';
                           'greens_cdu'='cdu_greens';
                           'greens_spd'='spd_greens'")

dta_coalitions <- dat_since1990_govparties %>% 
    mutate(coalition_option_stand = car::recode(government_parties, harmonise_governments))

dta_coalitions_merge <- dta_coalitions %>% 
    group_by(coalition_option_stand) %>% 
    summarise(coalitions_formed19902017_abs = sum(coalitions_formed19902017_abs))

sum(dta_coalitions_merge$coalitions_formed19902017_abs)

## save for analysis
write.csv(dta_coalitions_merge, "data_coalitions_laender.csv",
          row.names = FALSE)

## create nicer labels for plot
dta_coalitions_merge_plot <- dta_coalitions_merge %>% 
    mutate(coalition_option_print = str_to_upper(coalition_option_stand)) %>%
    mutate(coalition_option_print = str_replace_all(coalition_option_print, "SCHILL", "Schill")) %>% 
    mutate(coalition_option_print = str_replace_all(coalition_option_print, "LEFT", "Left")) %>% 
    mutate(coalition_option_print = str_replace_all(coalition_option_print, "GREENS", "Greens")) %>% 
    mutate(coalition_option_print = str_replace_all(coalition_option_print, "_", ", "))


## plot counts of coalitions for the appendix
ggplot(dta_coalitions_merge_plot, aes(x = reorder(coalition_option_print,
                                                  coalitions_formed19902017_abs),
                                      y = coalitions_formed19902017_abs)) +
    geom_bar(stat = "identity") + 
    coord_flip() +
    labs(x = NULL,
         y = "Number of coalitions on the Land level (1990-2017)")
ggsave("fig_a06.pdf",
       width = 8, height = 6)

