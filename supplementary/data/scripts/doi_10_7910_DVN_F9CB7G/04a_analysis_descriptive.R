#' ---
#' title: "Legislating Landlords: Private Interests, Issue Emphasis, and Policy Positions (LSQ)"
#' subtitle: "04a_analysis_descriptive.R"
#' author: "Authors: Stefan Mueller and Jihed Ncib"
#' date: "Note: Code compiled successfully on `r format(Sys.time(), '%d %B %Y')`"
#' ---

# script to merge and harmonise the text corpora and datasets on legislators
# and their interests

# If the code does not run, one or more packages may have been 
# updated, which may result in errors or conflicts. You can solve this issue
# by installing the package version listed above or by using the 
# groundhog package:
# after installing groundhog using install.packages("groundhog")
# change library(name_of_package) to
# groundhog::groundhog.library(name_of_package, date = "2024-05-06")
# Instead of adjusting the library() function for each package, 
# you can adjust them at all once using the
# the following syntax:
# groundhog.library("library('pkgA')
#                   library('pkgB')
#                   library('pkgC')", date = "2024-05-06")
# More details are available at: https://groundhogr.com/using/


library(tidyverse) # CRAN v2.0.0
library(Hmisc)     # CRAN v5.1-0
library(scales)    # CRAN v1.2.1

source("function_theme_base.R")


# Change in property prices

dat_daft_raw <- read.csv("daft_prices_2023q2_p10.csv",
                         fileEncoding = "utf-8")


dat_daft_long <- dat_daft_raw |> 
    gather(year, change, -c(Month)) |> 
    mutate(year = str_remove_all(year, "X")) |> 
    mutate(date = as.Date(paste("01", Month, year), format = "%d %B %Y")) |> 
    mutate(year = as.integer(year))


head(dat_daft_long)
dat_daft_year <- dat_daft_long |> 
    group_by(year) |> 
    summarise(mean_change_year = mean(change)) 

dat_daft_year

ggplot(filter(dat_daft_long, between(year, 2013, 2022)), # use period of investigation
       aes(x = as.Date(date), 
           y = change)) +
    geom_line(colour = "grey50") +
    geom_point(size = 1.8) +
    scale_x_date(breaks = "1 year", date_labels = "%Y") +
    labs(y = "Average Asking Price\nRelative to 2012 Average", x = NULL) +
    theme(panel.grid.major.y = element_line(colour = "grey70",
                                            linetype = "dotted"))
ggsave("fig_a01.pdf",
       width = 9, height = 4)
ggsave("fig_a01.eps",
       width = 9, height = 4)

# read parquet file
dat_analysis <- arrow::read_parquet("data_dontshare/data_analysis_classified_housing.parquet")

dat_analysis <- dat_analysis |> 
    filter(!is.na(ownership_2)) |> # remove observation without ownership status information
    mutate(housing_bert = dplyr::recode(housing_bert, "Housing" = 1, 
                                        "Non-housing" = 0)) |> 
    mutate(housing_bert = as.integer(housing_bert))

dat_analysis <- dat_analysis %>% 
    filter(between(year, 2013, 2022)) %>% 
    group_by(year, mpname) %>% 
    mutate(count_texts = n()) %>% 
    ungroup()


# bootstrap proportions (increase n_samples to get more samples)

n_samples <- 1000

dat_prop_year_boot <- dat_analysis %>% 
    filter(between(year, 2013, 2022)) %>% 
    group_by(type, year) %>% 
    do(data.frame(rbind(Hmisc::smean.cl.boot(.$housing_bert, na.rm = TRUE,
                                             B = n_samples)))) %>% 
    rename(prop_housing = Mean,
           prop_housing_95_lower = Lower,
           prop_housing_95_upper = Upper)


dat_prop_party_type_boot <- dat_analysis %>% 
    filter(between(year, 2013, 2022)) %>% 
    group_by(type, year, gov_opp, party_recoded) %>% 
    do(data.frame(rbind(Hmisc::smean.cl.boot(.$housing_bert, na.rm = TRUE,
                                             B = 4)))) %>% 
    rename(prop_housing = Mean,
           prop_housing_95_lower = Lower,
           prop_housing_95_upper = Upper)


levels_party <- c("Fine Gael",
                  "Fianna Fáil",
                  "Sinn Féin",
                  "Labour",
                  "Green Party", 
                  "AAA/S-PBP",
                  "Social Democrats",
                  "Other/Independents")

table(dat_prop_party_type_boot$year,
      dat_prop_party_type_boot$type)

dat_prop_party_type_boot_clean <- dat_prop_party_type_boot %>% 
    filter(!is.na(party_recoded)) %>% 
    mutate(party_recoded = factor(party_recoded, levels_party))


table(dat_analysis$year,
      dat_analysis$type)


dat_prop_party_type_boot_clean$type <- forcats::fct_rev(dat_prop_party_type_boot_clean$type)

# get average across year and facet
dat_means_party <- dat_prop_party_type_boot_clean |> 
    group_by(party_recoded, type) |> 
    summarise(mean = mean(prop_housing))

# Figure 03 ----
ggplot(dat_prop_party_type_boot_clean, aes(x = factor(year),
                                           y = prop_housing,
                                           colour = party_recoded)) +
    geom_point(aes(shape = gov_opp),
               size = 3.5) +
    geom_line(group = 1, linewidth = 1.05) +
    facet_grid(party_recoded~type) +
    scale_shape_manual(values = c(16, 15)) +
    geom_text(data = dat_means_party,
              aes(label = paste0("Average: ", sprintf("%.1f", 100 * round(mean, 3)), "%"),
                  x = 1, y = 0.2),
              colour = "grey40",
              size = 4.5, hjust = 0) +
    scale_y_continuous(limits = c(0, 0.25),
                       breaks = c(seq(0.0, 0.2, 0.1)),
                       labels = scales::percent_format(accuracy = 1)) +
    scale_colour_manual(values = c("Sinn Féin" = "#326760",
                                   "Fine Gael" = "#009FF3",
                                   "Fianna Fáil" = "#66BB66",
                                   "Labour" = "#CC0000",
                                   "AAA/S-PBP" = "#660000",
                                   "Social Democrats" = "#752F8B",
                                   "Green Party" = "#99CC33"),
                        guide = "none") +
    labs(x = "Year", y = "Percentage of Texts on Housing") +
    theme(axis.text.x = element_text(angle = 90),
          legend.position = "bottom",
          legend.title = element_blank(),
          strip.text.y = element_text(angle = 360, hjust = 0,
                                      face = "bold"))
ggsave("fig_03.pdf",
       width = 9, height = 11)
ggsave("fig_03.eps",
       width = 9, height = 11)



