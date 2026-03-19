# note --------------------------------------------------------------------

# this is one of the two replication scripts for:
#
# lena maria schaffer and resul umit, public support for national vs.
# international climate change obligations, journal of european public policy.
#
# the other replication file is called 01_analysis_stata.do
#
# running this script requires five third-party packages, which can be installed
# by removing the leading hashes from the lines 19 to 23 below.
#
#
# resul umit, 18 july 2022


# install the required libraries if they are not already installed --------

# if (!require("dplyr")) install.packages("dplyr")
# if (!require("tidyr")) install.packages("tidyr")
# if (!require("ggplot2")) install.packages("ggplot2")
# if (!require("nnet")) install.packages("nnet")
# if (!require("stargazer")) install.packages("stargazer")


# load the required libraries ---------------------------------------------

library(dplyr) # a grammar of data manipulation, cran v1.0.9
library(tidyr) # tidy messy data, cran v1.2.0
library(ggplot2) # create elegant data visualisations using the grammar of graphics, cran v3.3.6
library(nnet) # feed-forward neural networks and multinomial log-linear models, cran v7.3-17
library(stargazer) # well-formatted regression and summary statistics tables, cran v5.2.3


# import the datasets -----------------------------------------------------

# survey data
df_survey <- read.csv("03_data_survey.csv", fileEncoding = "latin1")

# official statistics
df_cantons <- read.csv("04_data_cantons.csv")
df_demographics <- read.csv("05_data_demographics.csv")


# figure 1 ----------------------------------------------------------------

# prepare the data and plot
df_survey %>%
        select(id, nie_support, nie_high) %>%
        pivot_longer(nie_support:nie_high,
                     names_to = "names",
                     values_to = "values") %>%
        group_by(names, values) %>%
        summarise(n = n()) %>%
        mutate(
                share = n * 100 / sum(n),
                categories = case_when(
                        names == "nie_support" & values == 1 ~ "Strongly\noppose",
                        names == "nie_support" &
                                values == 2 ~ "Oppose",
                        names == "nie_support" &
                                values == 3 ~ "Neither ...",
                        names == "nie_support" &
                                values == 4 ~ "Support",
                        names == "nie_support" &
                                values == 5 ~ "Strongly\nsupport",
                        names == "nie_support" &
                                is.na(values) ~ "Don't\nknow",
                        names == "nie_high" &
                                values == 1 ~ "Much\ntoo high",
                        names == "nie_high" &
                                values == 2 ~ "Too high",
                        names == "nie_high" &
                                values == 3 ~ "About\nright",
                        names == "nie_high" &
                                values == 4 ~ "Too low",
                        names == "nie_high" &
                                values == 5 ~ "Much\ntoo low",
                        names == "nie_high" &
                                is.na(values) ~ "Don't\nknow"
                ),
                names = factor(
                        names,
                        levels = c("nie_support", "nie_high"),
                        labels = c("Support / Oppose", "Low / High")
                ),
                values = ifelse(is.na(values), -9999, values)
        ) %>%
        ggplot(aes(x = share, y = reorder(categories, values))) +
        geom_bar(stat = "identity", position = "dodge") +
        facet_wrap(. ~ names, scales = "free_y") +
        theme_bw() + xlab("") + ylab("") +
        theme(
                axis.text = element_text(size = 13),
                axis.title = element_text(size = 13),
                strip.text = element_text(size = 13),
                legend.text = element_text(size = 13),
                legend.title = element_text(size = 13),
                legend.position = "bottom"
        ) +
        scale_x_continuous(
                labels = function(x)
                        paste0(x, "%")
        )

# save
ggsave(
        filename = "Figure1.png",
        dpi = 1000,
        width = 9,
        height = 6,
        units = "in"
)


# figure a1 ---------------------------------------------------------------

# calculate sample statistics
canton_sample_df <- df_survey %>%
        count(canton) %>%
        filter(!is.na(canton)) %>%
        mutate(prop = prop.table(n),
               source = "sample")

# calculate population statistics
canton_population_df <- df_cantons %>%
        group_by(canton) %>%
        summarise(n = sum(population)) %>%
        mutate(prop = prop.table(n),
               source = "population") %>%
        ungroup()

# merge and plot
rbind(canton_sample_df, canton_population_df) %>%
        ggplot(aes(
                x = reorder(canton, prop),
                y = prop,
                colour = source
        )) +
        geom_point(size = 3) +
        coord_flip() +
        labs(x = NULL, y = NULL, fill = "") +
        theme_minimal() +
        theme(
                axis.text = element_text(size = 14),
                axis.title = element_text(size = 14),
                legend.text = element_text(size = 14),
                legend.position = "top"
        ) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        scale_colour_grey(
                name = "",
                breaks = c("population", "sample"),
                labels = c("Population", "Sample"),
                start = 0.6,
                end = 0.2
        )

# save
ggsave(
        filename = "FigureA1.png",
        dpi = 1000,
        width = 9,
        height = 6,
        units = "in"
)



# figure a2 ---------------------------------------------------------------

# calculate sample statistics
as_sample_df <- df_survey %>%
        select(age, female) %>%
        filter(!(is.na(age) & is.na(female))) %>%
        mutate(female = recode(female, `0` = "Male", `1` = "Female")) %>%
        group_by(age, female) %>%
        summarise(n = n()) %>%
        ungroup() %>%
        mutate(share = prop.table(n),
               source = "sample") %>%
        select(age, female, share, source)

# calculate population statistics
as_population_df <-
        df_demographics %>%
        mutate(female = recode(gender, "man" = "Male", "woman" = "Female")) %>%
        mutate(source = "population") %>%
        select(age, female, share, source)

# merge and plot
rbind(as_sample_df, as_population_df) %>%
        ggplot(aes(x = age, y = share, fill = source)) +
        geom_density(stat = "identity", alpha = 0.5) +
        facet_wrap( ~ female) +
        labs(x = "\n\nAge", y = NULL, fill = "") +
        theme_bw() +
        theme(
                axis.text = element_text(size = 14),
                axis.title = element_text(size = 14),
                strip.text = element_text(size = 14),
                legend.text = element_text(size = 14),
                legend.position = "top"
        ) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        scale_fill_grey(
                breaks = c("population", "sample"),
                labels = c("Population", "Sample"),
                start = 0.6,
                end = 0.2
        )


# save
ggsave(
        filename = "FigureA2.png",
        dpi = 1000,
        width = 9,
        height = 6,
        units = "in"
)


# table a7 ----------------------------------------------------------------

# estimate the models
multinom_models <-
        multinom(
                nie_iv ~ age + female + education + income + climate + polinterest + leftright,
                data = df_survey,
                trace = FALSE
        )

# table the results
stargazer(
        multinom_models,
        type = "latex",
        title = "Multinomial logistic regression models as ransomisation check",
        covariate.labels = c(
                "Age",
                "Female",
                "Education",
                "Income",
                "Climate Worries",
                "Political Interest",
                "Left--Right",
                "Constant"
        ),
        keep.stat = c("n"),
        dep.var.caption = "",
        dep.var.labels.include = FALSE,
        column.labels = c("National", "International"),
        column.sep.width = "-5pt",
        header = FALSE,
        label = "randomisation_check",
        digits = 2,
        notes.append = FALSE,
        notes.align = "l",
        no.space = TRUE
) %>%
        capture.output(file = "TableA7.tex")
