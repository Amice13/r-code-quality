library(plyr)
library(dplyr)
library(here)

setwd(here())
setwd("./wikipedia")

languages <- "zh"

zh_dates <- c("2020-01-24", "2020-03-13")

## for (language in "ru") {
for (language in languages) {

    for_diff <- read.csv(paste0("data/agg_files/",language,"_page_views_2019_to_2020_by_category_",language,"_hist_leaders_pnas_check.csv")) # double check this file
    for_diff <- subset(
        for_diff,
        as.Date(date) < "2020-01-01" | as.Date(date) >= zh_dates[1]
    )

for_diff <- for_diff %>%
    filter(as.Date(date) <= zh_dates[2]) %>%
    mutate(
        sum_count = as.numeric(sum_count),
        before_after = as.numeric(as.Date(date) < zh_dates[1])
    ) %>%
    select(-date) %>%
    group_by(page, before_after) %>%
    summarise_all(mean) %>%
    ungroup() %>%
    arrange(page, desc(before_after))

top_diff <- for_diff %>%
    mutate(
        diff = sum_count - lag(sum_count),
        rel_diff = sum_count / lag(sum_count)
    ) %>%
    add_count(page) %>%
    filter(before_after != 1 & n==2) %>% # remove bad comparisons!!
    arrange(desc(rel_diff))

if (language %in% c("fa","zh")) {

top_blocked_diff <- for_diff %>%
    filter(blocked_pre_https > 0) %>%
    mutate(
        diff = sum_count - lag(sum_count),
        rel_diff = sum_count / lag(sum_count)
    ) %>%
    filter(before_after != 1) %>% # remove bad comparisons!!
    arrange(desc(rel_diff))
    }

top_leader_diff <- for_diff %>%
    filter(leader > 0) %>%
    mutate(
        diff = sum_count - lag(sum_count),
        rel_diff = sum_count / lag(sum_count)
    ) %>%
    filter(before_after != 1) %>% # remove bad comparisons!!
    arrange(desc(rel_diff))

top_hist_leader_diff <- for_diff %>%
    filter(historical_leader > 0) %>%
    mutate(
        diff = sum_count - lag(sum_count),
        rel_diff = sum_count / lag(sum_count)
    ) %>%
    filter(before_after != 1) %>% # remove bad comparisons!!
    arrange(desc(rel_diff))

getRelDiffs <- function(df) {
    paste0(
        as.character((df %>% arrange(desc(rel_diff)))$page)[1:10],
        " (",
        sprintf("%.2f", round(((df %>% arrange(desc(rel_diff)))$rel_diff)[1:10], 2)),
        ")"
    )
}

getDiffs <- function(df) {
    paste0(
        as.character((df %>% arrange(desc(diff)))$page)[1:10],
        " (",
        round(((df %>% arrange(desc(diff)))$diff)[1:10], 0),
        ")"
    )
}

if (language == "zh"){
xtable::xtable(
            cbind(
                getRelDiffs(top_diff),
                getRelDiffs(top_blocked_diff),
                getRelDiffs(top_leader_diff),
                getRelDiffs(top_hist_leader_diff)
            ),
            caption="Top relative increases for Wikipedia pages January 24 through March 13 compared to December 2019. Labels limited to: blocked, leader, historical leader, COVID/coronavirus.",
            row.names=F
        )

xtable::xtable(
            cbind(
                getDiffs(top_diff),
                getDiffs(top_blocked_diff),
                getDiffs(top_leader_diff),
                getDiffs(top_hist_leader_diff)
            ),
            caption="Top absolute increases for Wikipedia pages January 24 through March 13 compared to December 2019. Labels limited to: blocked, leader, historical leader, COVID/coronavirus.",
            row.names=F
)
}


    print(language)
    print(mean(data.frame(top_leader_diff)$rel_diff))
    print(mean(data.frame(top_hist_leader_diff)$rel_diff))
}

for_diff %>%
    filter(page=="2019冠状病毒病")
