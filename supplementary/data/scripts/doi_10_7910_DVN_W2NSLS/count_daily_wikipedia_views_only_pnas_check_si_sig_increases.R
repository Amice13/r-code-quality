library(dplyr)
library(readr)
library(data.table)
library(lubridate)
library(tidyr)
library(foreach)
library(doParallel)
library(here)

setwd(here())
setwd("./wikipedia")

stop("the original page view data must be collected from Wikipedia directly")

options(stringsAsFactors=F)

dates2 <- as.character(
    seq(
        as.Date("2019-12-01"),
        as.Date("2020-09-24"),
        by="day"
    )
)
languages2 <- c("ru","zh","de","it","fa")


source("code/count_daily_wikipedia_views_functions.R")

#### limited to only the political pages
fa_blocked_filtered <- read.csv(
    "data/ref_files/country_IR_wiki_fawiki_date_20130409_unavailable.csv",
    col.names=c("page","translation","description","topic","category")
)

unique_topic_names <- unique(fa_blocked_filtered$topic)
unique_topics <- gsub(
    "__", "_",
    sub("_$", "", gsub("\\.", "_", tolower(make.names(unique_topic_names))))
)
names(unique_topics) <- unique_topic_names

fa_blocked_filtered$topic <- unique_topics[fa_blocked_filtered$topic]

fa_blocked_filtered <- fa_blocked_filtered %>%
    filter(topic %in% c("human_rights","media_and_journalism","political")) %>%
    mutate(is_top_leader = 1)

ru_opposition <- read.csv(
    "data/ref_files/russian_opposition_media_human_rights_wiki_seeds_add_documentary_slogan_and_bbc_mentions.csv"
)

for (language in c("fa","ru")) {
    if (language=="fa") {
        BLOCKED <- unique(fa_blocked_filtered$page)
    } else if (language=="ru") {
        BLOCKED <- unname(
            sapply(
                unique(gsub("https://ru.wikipedia.org/wiki/", "", ru_opposition$name)),
                URLdecode
            )
        )
    } else {stop()}

    dates <- as.character(seq(as.Date(min(dates2)), as.Date(max(dates2)), by="day"))

    cores=detectCores()
    cl <- makeCluster(round(cores[1])) # all cores
    registerDoParallel(cl)
    ##
    all_list_counts <- foreach(
        i=dates, .combine=rbind,
        .packages=c("readr","dplyr","lubridate","tidyr")
    ) %dopar% {
        temp = getBlockedCounts(
            .language=language,
            .date = i,
            .blocked = unique(BLOCKED)
        )
        temp
    }
    stopCluster(cl)


    save(
        all_list_counts,
        file=paste0(
            "data/agg_files/",
            language,
            "_page_views_2019_to_2020_explore_politics",
            ifelse(language=="fa", "","_add_documentary_slogan_and_bbc_mentions"),
            ".RData"
        ),
        row.names=F
    )

}
