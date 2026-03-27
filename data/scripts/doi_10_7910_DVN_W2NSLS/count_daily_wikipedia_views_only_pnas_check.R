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
## languages2 <- c("zh")


source("code/count_daily_wikipedia_views_functions.R")

## standardize_wikipedia_pre_https_blocked_lists.R
load(
    "data/ref_files/zh_fa_pre_https_blocked_wikipedia_pages.RData"
)

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

## ## fix lines manually - 1459, 1380
## write.csv(
##     data.frame(leader_name = sapply(unname(sapply(unique(BLOCKED$page_title), URLdecode)), URLdecode), negative_case = 0),
##     file="data/ref_files/zh_pre_https_blocked_wikipedia_pages_wiki_seeds_combined_for_wiki2vec_pnas_check.csv", fileEncoding="UTF-8"
## )
## write.csv(
##     data.frame(leader_name = unname(fa_blocked), negative_case = 0),
##     file="data/ref_files/fa_pre_https_blocked_wikipedia_pages_wiki_seeds_combined_for_wiki2vec_pnas_check.csv"
## )

####
#### Analysis for main text

## combine_leader_wikipedia2vec_pnas_check_clean.R
load(
    "data/ref_files/zh_fa_it_de_ru_similar_wikipedia_pages_pnas_check.RData"
)
covid_df <- read.csv(
    "data/ref_files/covid_sars_wiki_seeds_with_pandemic.csv"
)
covid_df$wiki <- gsub(
    "https://[a-z][a-z].wikipedia.org/wiki/", "",
    sapply(covid_df$wikis, URLdecode)
)
for (language in languages2)  {
    cat("\n\n", toupper(language), "\n")
    BLOCKED <- NULL
    if (language == "zh") {
        BLOCKED <- zh_blocked_expanded## _df
    } else if (language == "fa") {
        BLOCKED <- fa_blocked_expanded_no_filter
    }
    LEADERS <- NULL
    if (language == "zh") {
        LEADERS_DF <- chinese_leaders_zh_wikis
    } else if (language == "fa") {
        LEADERS_DF <- iranian_leaders_fa_wikis
    } else if (language == "it") {
        LEADERS_DF <- italian_leaders_it_wikis
    } else if (language == "de") {
        LEADERS_DF <- german_leaders_de_wikis
    } else if (language == "ru") {
        LEADERS_DF <- russian_leaders_ru_wikis
    }
    HISTORICAL_LEADERS <- NULL
    HISTORICAL2013_LEADERS <- NULL
    if (language == "zh") {
        HISTORICAL2013_LEADERS_DF <- chinese2013_historical_leaders_zh_wikis
    } else if (language == "fa") {
        HISTORICAL2013_LEADERS_DF <- iranian2013_historical_leaders_fa_wikis
    } else if (language == "it") {
        HISTORICAL2013_LEADERS_DF <- italian2013_historical_leaders_it_wikis
    } else if (language == "ko") {
        HISTORICAL2013_LEADERS_DF <- korean2013_historical_leaders_ko_wikis
    } else if (language == "de") {
        HISTORICAL2013_LEADERS_DF <- german2013_historical_leaders_de_wikis
    } else if (language == "ru") {
        HISTORICAL2013_LEADERS_DF <- russian2013_historical_leaders_ru_wikis
    }
    COVID_DF <- subset(
        covid_df %>% rename(thelanguage = language),
        thelanguage == language
    )
    dates <- as.character(seq(as.Date(min(dates2)), as.Date(max(dates2)), by="day"))
    cores=detectCores()
    cl <- makeCluster(round(cores[1])) # cores
    registerDoParallel(cl)
    ##
    byday <- foreach(
        i=dates, .combine=rbind,
        .packages=c("readr","dplyr","lubridate")
    ) %dopar% {
        temp = readAndAggDailyPagecounts(
            .language=language,
            .date = i,
            .blocked_df = BLOCKED,
            .leaders_df = LEADERS_DF,
            .historical2013_leaders_df = HISTORICAL2013_LEADERS_DF,
            .covid_df = COVID_DF
        )
        temp
    }
    stopCluster(cl)
    cat("\n    Writing to file..\n")
    write.table(
        byday,
        file=paste0(
            "data/agg_files/",
            language, "_wikipedia_page_views_blocked_v_not_leaders_v_not_",
            as.character(min(dates2)), "_to_", as.character(max(dates2)),
            "_pnas_check.csv"
        ),
        sep="\t",
        row.names=F
    )
}

####
#### Analysis for SI

dates2 <- as.character(
    seq(
        as.Date("2019-12-01"),
        as.Date("2020-09-24"),
        by="day"
    )
)

filename_map <- c(
    "zh_blocked_expanded" = "zh_pre_https_blocked_wikipedia_pages",
    "zh_blocked_expanded_test" = "zh_pre_https_blocked_wikipedia_pages_test",
    ##
    "fa_blocked_expanded" = "fa_pre_https_blocked_wikipedia_pages_political_plus",
    "fa_blocked_expanded_no_filter" = "fa_pre_https_blocked_wikipedia_pages",
    "fa_significant_increases" = "fa_significant_increases",
    ##
    "russian_dissidents_navalny_only_no_putin_diff" = "russian_dissidents_navalny_only_no_putin_diff",
    "ru_significant_increases_add_bbc_mentions" = "ru_significant_increases_add_bbc_mentions"
)

language_map <- c(
    "zh_blocked_expanded" = "zh",
    "zh_blocked_expanded_test" = "zh",
    ##
    "fa_blocked_expanded" = "fa",
    "fa_blocked_expanded_no_filter" = "fa",
    "fa_significant_increases" = "fa",
    ##
    "russian_dissidents_navalny_only_no_putin_diff" = "ru",
    "ru_significant_increases_add_bbc_mentions" = "ru"
)


all_blocked_counts <- list()

for (thefile in names(filename_map)) {
    print(thefile)
    ##
    dates <- as.character(seq(as.Date(min(dates2)), as.Date(max(dates2)), by="day"))
    cores=detectCores()
    cl <- makeCluster(round(cores[1])) # half of cores
    registerDoParallel(cl)
    BLOCKED_DF <- get(thefile)
    ##
    all_list_counts <- foreach(
        i=dates, .combine=rbind,
        .packages=c("readr","dplyr","lubridate","tidyr")
    ) %dopar% {
        temp = getBlockedCounts(
            .language=language_map[thefile],
            .date = i,
            .blocked = NULL,
            .blocked_df = BLOCKED_DF
        )
        temp
    }
    stopCluster(cl)
    ##
    all_blocked_counts[[thefile]] <- all_list_counts
    rm(all_list_counts)
    ##
}

save(
    all_blocked_counts,
    file=paste0(
        "data/agg_files/all_blocked_page_counts_2019_to_2020.RData"
    )
)

dates2 <- as.character(
    seq(
        as.Date("2019-12-01"),
        as.Date("2020-04-30"),
        by="day"
    )
)


####
#### SI top page increases (Chinese)

language <- "zh"

theblocked <- zh_blocked
theleaders <- subset(
    chinese_leaders_zh_wikis,
    is_top_leader==1
)$leader_name
thehistleaders <- subset(
    chinese2013_historical_leaders_zh_wikis,
    is_top_leader==1
)$leader_name
thecovid <- subset(
    covid_df,
    language=="zh"
    & !grepl(URLencode("2019冠状病毒病疫情"), wikis) # SI table calculated before this addition
)$wiki

BLOCKED <- c(
    theblocked,
    theleaders,
    thehistleaders,
    thecovid
)

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

all_list_counts <- all_list_counts %>%
    mutate(
        blocked_pre_https = as.integer(page %in% theblocked),
        leader = as.integer(page %in% theleaders),
        historical_leader = as.integer(page %in% thehistleaders),
        covid_etc = as.integer(page %in% thecovid),
        rest_of_wikipedia = as.integer(page %in% c("rest of wikipedia"))
    )

write.csv(
    all_list_counts,
    file=paste0(
        "data/agg_files/",
        language,"_page_views_2019_to_2020_by_category_fixed_",
        language,"_hist_leaders_pnas_check.csv"
       ),
    row.names=F
)
