########
### Authors: James Cross, Derek Greene, Stefan Müller, and Martijn Schoonvelde 
### "Mapping Digital Campaign Strategies: 
### How Political Candidates Use Social Media to Communicate Constituency Connection and Policy Stance.
### Computational Communication Research.
### This script contains the code to produce the figures and tables for the descriptive analysis
### All datasets needed to run this script are available in the repository.
#######

# Load packages
library(cowplot)    # CRAN v1.1.3
library(tidyverse)  # CRAN v2.0.0
library(readr)      # CRAN v2.1.5
library(texreg)     # CRAN v1.39.4
library(xtable)     # CRAN v1.8-4
library(ggeffects)  # CRAN v2.3.0
library(haven)      # CRAN v2.5.4
library(scales)     # CRAN v1.3.0
library(forcats)    # CRAN v1.0.0
library(ggbeeswarm) # CRAN v0.7.2

# Set custom ggplot2 theme
theme_baser <- function() {
    theme_minimal() %+replace%
        theme(
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.border = element_rect(
                fill = NA, color = "black", linewidth = 0.5,
                linetype = "solid"
            ),
            # panel.border = element_rect(color = "black", fill = NA, size = 1),
            legend.title = element_text(size = 15),
            title = element_text(size = 15, vjust = 1.5, hjust = 0),
            legend.position = "bottom",
            plot.title = element_text(
                size = 17, face = "bold",
                vjust = 1.5, hjust = 0.5,
                margin = margin(t = 15, r = 0, b = 0, l = 0)
            ),
            axis.ticks = element_line(linewidth = 0.3),
            axis.ticks.length = unit(0.3, "cm"),
            legend.text = element_text(size = 13),
            strip.text = element_text(
                size = 15, hjust = 0.5,
                margin = margin(b = 5, r = 5, l = 5, t = 5)
            ),
            axis.text = element_text(colour = "black", size = 13),
            axis.title = element_text(size = 13, hjust = 0.5)
        )
}


theme_set(theme_baser())


# load data for Figure 2 
dat_fig_02 <- read_csv("data_fig_02.csv")

# nicer labels
dat_fig_02 <- dat_fig_02 %>%
    mutate(label = paste0(
        party_recoded, ": ",
        percent(prop_accounts, accuracy = 0.1)
    ))


ggplot(dat_fig_02, aes(
    x = prop_accounts,
    y = reorder(party_recoded, prop_accounts)
)) +
    geom_bar(stat = "identity", fill = "black", width = 0.8) +
    geom_text(aes(label = label),
        hjust = 1, nudge_x = -0.01,
        size = 4.5, colour = "white"
    ) +
    scale_x_continuous(
        labels = scales::percent_format(accuracy = 1),
        limits = c(0, 1),
        expand = expansion(mult = c(0, 0.025)),
        breaks = seq(0, 1, 0.2)
    ) +
    labs(x = "Percentage of candidates with Twitter account", y = NULL) +
    theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none"
    )
ggsave("fig_02.pdf",
    width = 9, height = 3.5
)


# load keyness data
dat_keyness_combined <- read_csv("data_keyness_terms.csv")

# print table with keyness analysis
print(
    xtable(dat_keyness_combined,
        digits = 1,
        caption = "Keyness analysis for each category. Table lists the 30 most important words and hashtags distinguishing tweets classified into the target category from tweets classified into the three other categories (reference categories).",
        label = "tab:keyness",
        align = c(
            "p{0.03\\textwidth}",
            "p{0.12\\textwidth}",
            "p{0.3\\textwidth}",
            "p{0.45\\textwidth}"
        )
    ),
    type = "latex",
    digits = 1,
    size = "footnotesize",
    file = "tab_01.tex",
    include.rownames = FALSE,
    caption.placement = "top"
)


# load data for analysis
dat <- haven::read_dta("data_analysis.dta")

# number of tweets
nrow(dat)

# get summary statistics for tweets over time
dat_sum <- dat |>
    group_by(screen_name_merge, candidate_merge, party, birth, seniority) %>%
    mutate(prediction_policy_lr_num = ifelse(prediction_policy_lr == "policy", 1, 0)) %>%
    mutate(prediction_electioneering_lr_num = ifelse(prediction_electioneering_lr == "electioneering", 1, 0)) %>%
    summarise(
        mean_policy = mean(prediction_policy_lr_num),
        mean_electioneering = mean(prediction_electioneering_lr_num)
    ) %>%
    mutate(birth = as.numeric(birth))

dat_sum <- dat_sum %>%
    gather(type_tweet_mean, value, -c(candidate_merge, screen_name_merge, party, birth, seniority)) %>%
    mutate(type_tweet_mean = dplyr::recode(type_tweet_mean,
        "mean_electioneering" =
            "Electioneering Tweets",
        "mean_policy" = "Policy Tweets"
    ))


dat_cor_birth <- dat_sum %>%
    group_by(type_tweet_mean) %>%
    summarise(cor = cor(value, birth, use = "pairwise.complete.obs"))

# Figure A3
ggplot(dat_sum, aes(x = birth, y = value)) +
    geom_point(alpha = 0.6) +
    geom_smooth() +
    facet_wrap(~type_tweet_mean) +
    scale_y_continuous(
        labels = scales::percent_format(accuracy = 1)#,
      #  limits = c(0, 1)
    ) +
    geom_text(
        data = dat_cor_birth, aes(label = paste0("r=", round(cor, 2))),
        x = 1985, y = 0.9,
        size = 5, colour = "grey50"
    ) +
    labs(x = "Year of Birth", y = "Percentage of Tweets")
ggsave("fig_a03.pdf",
    width = 9, height = 4
)



# get tweets per day for each category
dat_day <- dat |>
    group_by(date, dv_categories) |>
    count()


dat_day$dv_categories <- factor(dat_day$dv_categories,
    levels = c(
        "Electioneering",
        "Policy",
        "Both",
        "Neither"
    )
)


# Ensure dv_categories has the correct factor levels
dat_day$dv_categories <- factor(dat_day$dv_categories,
    levels = c("Electioneering", "Policy", "Both", "Neither")
)


# Pull target y values

# Updated label positions
label_df <- data.frame(
    dv_categories = factor(c("Electioneering", "Policy", "Electioneering"),
        levels = c("Electioneering", "Policy", "Both", "Neither")
    ),
    x = as.Date(c("2020-01-05", "2019-09-30", "2020-02-02")),
    y = c(
        300,
        520,
        800
    ),
    label = c("Election\ncalled", "Budget\nday", "Election")
)

# create plot with labels
ggplot(dat_day, aes(x = as.Date(date), y = n, colour = dv_categories)) +
    geom_point(alpha = 0.6, size = 2, shape = 16) +
    facet_wrap(~dv_categories, nrow = 1) +
    geom_smooth(method = "loess", se = FALSE, colour = "white", linewidth = 2) +
    geom_smooth(method = "loess", se = FALSE) +
    scale_colour_manual(values = c(
        "Policy" = "#a2a2a2",
        "Electioneering" = "black",
        "Both" = "#6fb2e4",
        "Neither" = "darkgreen"
    )) +
    scale_x_date(date_labels = "%b '%y", breaks = "1 month") +
    labs(x = NULL, y = "Number of Tweets per day") +
    theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "none"
    ) +
    geom_vline(
        xintercept = as.Date("2020-02-08"),
        linetype = "dashed",
        colour = "grey50"
    ) +
    geom_text(
        data = label_df,
        aes(x = x, y = y, label = label),
        colour = "grey30",
        hjust = 1,
        size = 4,
        inherit.aes = FALSE
    )
# Save plot
ggsave("fig_05.pdf", width = 9, height = 5)


# load data with candidate information

dat_candidates <- read_csv("data_candidates_ge2020.csv")

# get Twitter IDs for merging
dat_candidates$screen_name_merge <- str_to_lower(dat_candidates$twitter_id)

# from full dataset get Twitter IDs
dat_tweets_account <- dat |>
    group_by(screen_name_merge) |>
    count() |>
    rename(n_tweets = n)

# get probabilities
# 1 divided by (the fractional odds plus 1),
# multiplied by 100 to give a percentage.
# e.g. fractional odds of 3/1 = (1 / ((3/1) + 1)) * 100 = 25%.

dat_candidates <- dat_candidates |>
    mutate(paddy_power_odds = ifelse(paddy_power_odds == "EVS", "1/1",
        paddy_power_odds
    )) |>
    separate(paddy_power_odds,
        into = c(
            "paddy_power_numerator",
            "paddy_power_denominator"
        ),
        sep = "/", remove = FALSE
    ) |>
    mutate(
        paddy_power_numerator = as.numeric(paddy_power_numerator),
        paddy_power_denominator = as.numeric(paddy_power_denominator)
    ) |>
    mutate(paddy_power_percentage = (1 / ((paddy_power_numerator / paddy_power_denominator) + 1)) * 100) |>
    mutate(paddy_power_probs = paddy_power_percentage / 100)


dat_joined <- left_join(dat_candidates, dat_tweets_account)

dat_tweeters <- dat_joined |>
    filter(!is.na(n_tweets)) |>
    mutate(party_recoded = case_when(
        party == "SD" ~ "Social Democrats",
        party == "FF" ~ "Fianna Fáil",
        party == "FG" ~ "Fine Gael",
        party == "LAB" ~ "Labour",
        party == "SF" ~ "Sinn Féin",
        party == "GP" ~ "Greens",
        party == "SOL-PBP" ~ "Solidarity-PBP",
        TRUE ~ "Other/Independent"
    ))


dat_joined <- dat_joined |>
    group_by(constituency_name) |>
    mutate(paddy_power_prob_max = max(paddy_power_probs, na.rm = TRUE)) |>
    mutate(competitiveness = 1 + paddy_power_probs - paddy_power_prob_max) |>
    mutate(twitter_user = ifelse(!is.na(n_tweets), "Candidates with Twitter account",
        "Candidates without Twitter account"
    )) |>
    mutate(independent_cand = ifelse(party == "IND", "Independent", "Party Candidates")) |>
    filter(!is.na(competitiveness)) # no odds for six politicians available

ggplot(dat_joined, aes(x = competitiveness)) +
    geom_histogram() +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 45)) +
    facet_wrap(~twitter_user, nrow = 1) +
    labs(x = "Candidate competitiveness", y = "Number of candidates")
ggsave("fig_06.pdf",
    width = 9, height = 4
)



# get candidate information
dat_candidate <- dat |>
    group_by(
        candidate, party, gender, competitiveness,
        experience_running_dummy, dv_categories
    ) |>
    count() |>
    group_by(candidate) |>
    mutate(prop = n / sum(n)) |>
    filter(party != "") #|>
  #  filter(dv_categories %in% c("Electioneering", "Policy"))


dat_candidate$dv_categories <- factor(dat_candidate$dv_categories,
    levels = c(
        "Electioneering",
        "Policy",
        "Both",
        "Neither"
    )
)

# create competitiveness quartiles
dat_candidate <- dat_candidate |>
    filter(!is.na(competitiveness)) |>
    group_by(dv_categories) |>
    mutate(comp_quartiles = ntile(competitiveness, 3)) |>
    mutate(comp_quartiles = dplyr::recode(
        comp_quartiles,
        "1" = "Low",
        "2" = "Medium", "3" = "High"
    )) |> 
    filter(dv_categories %in% c("Electioneering", "Policy")) 

dat_candidate$comp_quartiles <- factor(
    dat_candidate$comp_quartiles,
    levels = c(
        "Low",
        "Medium",
        "High"
    )
)


p_comp <- ggplot(dat_candidate, aes(
    x = fct_rev(comp_quartiles), y = prop,
    colour = fct_rev(comp_quartiles)
)) +
    geom_boxplot(outlier.colour = "white") +
    ggbeeswarm::geom_quasirandom(alpha = 0.2) +
    coord_flip() +
    scale_colour_grey(start = 0.3, end = 0.7) +
    facet_wrap(~dv_categories, nrow = 1) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
        y = "Percentage of Tweets", x = NULL,
        title = "Competitiveness"
    ) +
    theme(legend.position = "none")
p_comp

# prevalence by gender
p_gender <- ggplot(dat_candidate, aes(
    x = gender, y = prop,
    colour = gender
)) +
    geom_boxplot(outlier.colour = "white") +
    ggbeeswarm::geom_quasirandom(alpha = 0.2) +
    coord_flip() +
    scale_colour_manual(values = c("black", "grey50")) +
    facet_wrap(~dv_categories) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
        y = "Percentage of Tweets", x = NULL,
        title = "Gender"
    ) +
    theme(legend.position = "none")
p_gender

dat_candidate <- dat_candidate |>
    mutate(experience_running_dummy = dplyr::recode(
        experience_running_dummy,
        "0" = "First campaign", "1" = "Ran previously"
    )) |>
    mutate(experience_running_dummy = fct_rev(experience_running_dummy))

# prevalence by experience
p_exp <- ggplot(dat_candidate, aes(
    x = fct_rev(experience_running_dummy), y = prop,
    colour = fct_rev(experience_running_dummy)
)) +
    geom_boxplot(outlier.colour = "white") +
    ggbeeswarm::geom_quasirandom(alpha = 0.2) +
    coord_flip() +
    scale_colour_manual(values = c("grey50", "black")) +
    facet_wrap(~dv_categories) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
        y = "Percentage of Tweets", x = NULL,
        "Incumbent", title = "Experience"
    ) +
    theme(legend.position = "none")
p_exp


# combine plots
cowplot::plot_grid(p_comp, p_exp, p_gender,
    nrow = 3,
    align = "vh"
)
ggsave("fig_07.pdf",
    width = 9, height = 8
)

### Figures and Tables for Appendix

dat_competitiveness <- read_csv("data_reg_competitive.csv")

# run simple logistic regression model
glm_1 <- glm(elected_num ~ competitiveness,
    data = dat_competitiveness,
    family = binomial(link = "logit")
)

glm_2 <- glm(elected_num ~ competitiveness + party,
    data = dat_competitiveness,
    family = binomial(link = "logit")
)


# print regression tables
screenreg(list(glm_1, glm_2))

# save Table A5
texreg(list(glm_1, glm_2),
    include.ci = FALSE,
    caption.above = TRUE,
    custom.coef.names = c(
        "(Intercept)",
        "Competitiveness",
        "Party: Fianna Fáil (ref.: Aontú)",
        "Party: Fine Gael",
        "Party: Green Party",
        "Party: Labour",
        "Party: Other/Independent Candidates",
        "Party: Sinn Féin",
        "Party: Social Democrats",
        "Party: Solidarity-PBP"
    ),
    include.variance = FALSE,
    caption = "Predicting  whether a candidate obtains a seat, conditional on competitiveness and a candidate's party affiliation. The models include all candidates with an active Twitter account.",
    fontsize = "small",
    label = "tab:reg_competitive",
    file = "tab_a05.tex"
)

# get predicted probabilities for competitiveness
pred_odds <- ggpredict(glm_2, terms = "competitiveness [0:1 by=0.05]")

pred_odds

# Figure A1
ggplot(pred_odds, aes(
    x = x, y = predicted,
    ymin = conf.low,
    ymax = conf.high
)) +
    geom_histogram(
        data = dat_competitiveness,
        aes(
            x = competitiveness,
            after_stat(count) / 100
        ),
        bins = 80,
        colour = "grey30",
        fill = "white",
        inherit.aes = FALSE,
        alpha = 0.8
    ) +
    geom_ribbon(fill = "grey50", alpha = 0.6) +
    geom_line(linewidth = 0.8) +
    labs(x = "Competitiveness", y = "Pr(Winning a seat)")
ggsave("fig_a01.pdf",
    width = 9, height = 5
)


# Save Figure A2
ggplot(dat_candidate, aes(
    x = fct_rev(party), y = prop,
    colour = party
)) +
    geom_boxplot(outlier.colour = "white") +
    ggbeeswarm::geom_quasirandom(alpha = 0.2) +
    coord_flip() +
    scale_colour_manual(values = c(
        "Sinn Féin" = "#326760",
        "Fine Gael" = "#009FF3",
        "Fianna Fáil" = "#66BB66",
        "Labour" = "#CC0000",
        "Social Democrats" = "#752F8B",
        "Solidarity-PBP" = "#8E2420",
        "Aontú" = "#44532A",
        "Green Party" = "#99CC33"
    )) +
    facet_wrap(~dv_categories) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(y = "Percentage of Tweets", x = NULL) +
    theme(legend.position = "none")
ggsave("fig_a02.pdf",
    width = 9, height = 6
)

