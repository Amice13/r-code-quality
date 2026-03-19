#################################################################################################
## This program calculates the cost concentrations that were input into the simulation.
##
## This program DOES NOT need to be rerun unless you wanted to alter these cost concentrations (
## currently 20/80; 11/89; and 5/95), or adjust assumptions about household size or the 80/20 
## rule. 
#################################################################################################

#clear workspace
rm(list=ls())

library(readxl)
library(tidyverse)
library(pander)
library(scales)

options(stringsAsFactors = FALSE)
options(digits = 6)

#################################
## Define inputs ##
#################################

#Data 
input_data <- "2019-Actuarial-Value-Calculator (1).xlsm"

#Multiplier to obtain average cost per household
#Default is 2.58 people per household [2010 U.S. Census](https://www.census.gov/prod/cen2010/briefs/c2010br-14.pdf)]
input_avgHHsize <- 2.58
    
#Multiplier to obtain average price per household
#Default is 1.25 (multiplied by average household size) [80/20 Rule](https://www.healthcare.gov/health-care-law-protections/rate-review/)]
input_8020rule <- 1.25

#################################
## Select cuts (one at a time) ##
## These cut points reflect    ##
## the proportion of high risk ##
## population                  ##
#################################

#For the 20/80 cost concentration
input_cut <- .2

#For the 11/89 cost concentration
#input_cut <- .11

#For the 5/95 cost concentration
#input_cut <- .05

#################################
## Prepare functions ##
#################################

levels <- c("Bronze", "Silver", "Gold", "Platinum")

read_sheet <- function(level) {
    sheet <- paste0(level, "_Combined")
    data <- read_excel(input_data, sheet, skip = 3)
    data$level <- level
    return(data)
}

read_sheets <- function() {
    blocks <- lapply(levels, read_sheet)
    bind_rows(blocks)
}

calculate_proportions <- function(data) {
    n <- sym("Number of Enrollees")
    cost <- sym("Avg. Cost per Enrollee (Bucket)")

    data %>%
        select(level, !!n, !!cost) %>%
        group_by(level) %>%
        mutate(
            p_pop = !!n / sum(!!n)
        ) %>%
        arrange(desc(!!cost)) %>%
        mutate(
            cum_pop = cumsum(p_pop),
            average_cost = cumsum(!!n * !!cost) / cumsum(!!n)
        ) %>%
        ungroup() %>%
        select(level, cum_pop, average_cost)
}

average_cost <- function(data, cut, debug = FALSE) {
    equal <- data %>%
        filter(cum_pop == cut)
    if (nrow(equal) > 0) {
        return(equal %>% pull(average_cost))
    }

    below <- data %>%
        filter(cum_pop < cut) %>%
        mutate(d = cum_pop - cut) %>%
        arrange(desc(d)) %>%
        head(1)

    above <- data %>%
        filter(cum_pop > cut) %>%
        mutate(d = cum_pop - cut) %>%
        arrange(d) %>%
        head(1)

    if (debug) {
        bind_rows(below, above) %>%
            print()
    }

    a_x <- below %>% pull(cum_pop)
    a_y <- below %>% pull(average_cost)

    b_x <- above %>% pull(cum_pop)
    b_y <- above %>% pull(average_cost)

    slope <- (b_y - a_y) / (b_x - a_x)

    interpolated <- a_y + slope * (cut - a_x)
    return(interpolated)
}

average_costs <- function(data, cuts) {
    data.frame(
        cut = cuts,
        average_cost = sapply(cuts, function(cut) average_cost(data, cut))
    )
}

costs_table <- function(stats) {
    top <- stats %>%
        group_by(level) %>%
        do(average_costs(., c(input_cut, 1))) %>%
        ungroup() %>%
        spread(key = level, value = average_cost) %>%
        select(cut, Bronze, Silver, Gold, Platinum)

    ## add corresponding bottom groups
    for (i in 1) {
        row <- top[i, ]
        c <- row$cut
        average <- top %>% filter(cut == 1)
        new_row <- (average - c * row) / (1 - c)
        new_row$cut <- 1 - c
        top <- bind_rows(top, new_row)
    }

    tibble(
        "Cost Group" = c(paste0("Top ",input_cut*100,"%"), "All", paste0("Bottom ",(1-input_cut)*100,"%"))
    ) %>%
        bind_cols(top) %>%
        select(-cut) %>%
        mutate_if(is.numeric, my_dollar)
}

my_dollar <- function(x) dollar(round(x))

###################################
## Read in data and prepare data ##
###################################

raw <- read_sheets()
stats <- calculate_proportions(raw)

# calculate average cost per household 
stats <- stats %>%
    mutate(average_cost = input_avgHHsize * average_cost) #Average cost per household = 2.58 times Average cost per enrollee.

# calculate average price per household
stats <- stats %>%
    mutate(average_cost = input_8020rule * average_cost) #Average price per household 1.25 times$ Average cost per household. 

#################################
## output cost concentrations ##
#################################
stats %>%
    costs_table() %>%
    pandoc.table(caption = "Cost Concentrations, Price per Household by Cost Group")