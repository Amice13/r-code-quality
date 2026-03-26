library(tidyverse)
library(stm)
library(xtable)

library(here)

setwd(here())
setwd("./twitter")

load("data/all_topic_estimates_fixed_categories_ats_removed_add_woguo_and_provinces.RData")

load("data/n_country_mentions_fixed_categories_ats_removed_add_woguo.RData")

models <- models %>%
    setNames(
        c("mentions_china","mentions_any_country","all_tweets")
    )

createKeywordTable <- function(.models, .which_set=1) {

    this_topic_model <- .models[[.which_set]]$topic_model


    topics_cn <- labelTopics(
        this_topic_model, n = 10
    )

    topic_keywords <- topics_cn[[1]] %>% # highest prob
        data.frame() %>%
        unite("keywords", X1:X10, sep=", ")

    topic_proportions <- this_topic_model$theta %>%
        data.frame() %>%
        summarise(
            across(everything(), mean)
        )

    topic_table <- data.frame(
        translation = "",
        keywords = topic_keywords,
        proportions = t(topic_proportions)
    ) %>% arrange(desc(proportions)) %>%
        mutate(
            proportions = round(proportions, 2)
        )

    print(
    xtable(
        topic_table
    ),
    row.names=F,
    include.rownames=F,
    include.colnames=F
)

}

sink("china_topic_model.tex")
createKeywordTable(
    .models = models, # politics not state media, 2019-12 on
    .which_set = 1 # china
)
sink()

sink("countries_topic_model.tex")
createKeywordTable(
    .models = models, # politics not state media, 2019-12 on
    .which_set = 2 # any country
)
sink()
