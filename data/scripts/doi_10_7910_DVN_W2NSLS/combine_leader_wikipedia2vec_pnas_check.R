library(dplyr)
library(here)

setwd(here())
setwd("./wikipedia/data/ref_files")

options(stringsAsFactors=F)

formatLeaders <- function(nationality, language, entities_only=TRUE) {
    top_leaders <- gsub(" ", "", gsub(
        paste0("https://",language,".wikipedia.org/wiki/"), "",
        read.csv(
            paste0(
                nationality,
                "_leader_names_and_wiki_pages_checked_february.csv"
            ),
            comment.char="#"
        )$native_wiki
    )
        )
    wiki2vecfile <- paste0(
        nationality, "_leaders_similar_wikis_",
        language,"_pnas_check.csv"
    )
    if (file.exists(wiki2vecfile)) {
    leaders <- read.csv(
        wiki2vecfile
    )$word
    } else {
        leaders <- NULL
        }
    leaders_df <- data.frame(
        leader_name=c(
            top_leaders,
            leaders
        )
    )
    leaders_df$is_top_leader <- as.integer(
        leaders_df$leader_name %in% top_leaders
    )
    ##
    if (entities_only) {
    leaders_df <- subset(
        leaders_df,
        grepl("ENTITY", leader_name) | is_top_leader
    )
    }
    leaders_df$leader_name <- sapply(
        gsub("ENTITY/", "", leaders_df$leader_name),
        URLdecode
    )
    return(leaders_df)
}

formatHistoricalLeaders <- function(nationality, language, entities_only=TRUE) {
    top_leaders <- gsub(" ", "", gsub(
        paste0("https://",language,".wikipedia.org/wiki/"), "",
        subset(read.csv(
            paste0("historical_", nationality, "_leaders_wiki_seeds.csv"),
            comment.char="#"
        ), current == 0)$wikis
    )
        )
    wiki2vecfile <- paste0(
        nationality, "_historical_leaders_similar_wikis_",
        language,"_pnas_check.csv"
    )
    if (file.exists(wiki2vecfile)) {
    leaders <- read.csv(
        wiki2vecfile
    )$word
    } else {
        leaders <- NULL
        }
    leaders_df <- data.frame(
        leader_name=c(
            top_leaders,
            leaders
        )
    )
    leaders_df$is_top_leader <- as.integer(
        leaders_df$leader_name %in% top_leaders
    )
    ##
    if (entities_only) {
    leaders_df <- subset(
        leaders_df,
        grepl("ENTITY", leader_name) | is_top_leader
    )
    }
    leaders_df$leader_name <- sapply(
        gsub(
            "https://[a-z][a-z].wikipedia.org/wiki/", "",
            gsub("ENTITY/", "", leaders_df$leader_name)
        ),
        URLdecode
    )
    return(leaders_df)
}


iranian_leaders_fa_wikis <- formatLeaders("iranian",language="fa")
chinese_leaders_zh_wikis <- formatLeaders("chinese",language="zh")
italian_leaders_it_wikis <- formatLeaders("italian",language="it")
german_leaders_de_wikis <- formatLeaders("german",language="de")
russian_leaders_ru_wikis <- formatLeaders("russian",language="ru")

iranian2013_historical_leaders_fa_wikis <- formatHistoricalLeaders(
    "iranian2013","fa"
)
chinese2013_historical_leaders_zh_wikis <- formatHistoricalLeaders(
    "chinese2013","zh"
)
italian2013_historical_leaders_it_wikis <- formatHistoricalLeaders(
    "italian2013","it"
    )
german2013_historical_leaders_de_wikis <- formatHistoricalLeaders(
    "german2013","de"
    )
russian2013_historical_leaders_ru_wikis <- formatHistoricalLeaders(
    "russian2013","ru"
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

for (thefile in names(filename_map)) {
    .blocked <- read.csv(
        paste0(
            filename_map[thefile],"_similar_wikis_",
            language_map[thefile],"_pnas_check.csv"
        )
    )$word
    .blocked_seeds <- subset(
        read.csv(
            paste0(
                ## "data/ref_files/",
                filename_map[thefile],
                "_wiki_seeds_combined_for_wiki2vec_pnas_check.csv"
            )
        ), negative_case==0
    )$leader_name
    ##
    ._blocked_df <- data.frame(
        leader_name=c(
            .blocked_seeds,
            .blocked
        )
    )
    ._blocked_df$is_top_leader <- as.integer(
        ._blocked_df$leader_name %in% .blocked_seeds
    )
    ##
    ._blocked_df <- subset(
        ._blocked_df,
        grepl("ENTITY", leader_name) | is_top_leader
    )
    ._blocked_df$leader_name <- sapply(
        gsub(
            "https://[a-z][a-z].wikipedia.org/wiki/", "",
            gsub("ENTITY/", "", ._blocked_df$leader_name)
        ),
        URLdecode
    )
    assign(thefile, ._blocked_df)
}



## covids <- list()
## covids_all <- list()
## seeds <- read.csv("covid_sars_wiki_seeds_with_pandemic.csv")
## seeds$wikis <- sapply(
##         gsub(
##             "https://[a-z][a-z].wikipedia.org/wiki/", "",
##             seeds$wikis
##         ),
##     URLdecode
## )
## for (.language in c("zh","it","fa","de","ru")) {
##         ## paste0("covid_sars_similar_wikis_", .language,".csv")
##     covids[[.language]] <- rbind(
##         read.csv(
##             paste0(.language,"_covid_similar_wikis_",.language,"_pnas_check.csv")
##         ) %>% mutate(year = "2019"),
##         read.csv(
##             paste0(.language,"_covid_similar_wikis_",.language,"_pnas_check_2020.csv")
##         ) %>% mutate(year = "2020")
##     )
##     ## covids[[.language]]$is_seed <- as.integer(
##     ##     covids[[.language]]$word %in% seeds
##     ##     )
##     covids_all[[.language]] <- rbind(
##         subset(
##             seeds, language == .language
##         ) %>% rename(word = wikis) %>%
##         mutate(score=NA, year=NA) %>% select(-language),
##         covids[[.language]]
##     )
##     covids[[.language]] <- rbind(
##         subset(
##             seeds, language == .language
##         ) %>% rename(word = wikis) %>%
##         mutate(score=NA, year=NA) %>% select(-language),
##         subset(
##             covids[[.language]],
##             grepl("ENTITY", word)
##             )
##     )
## }
## covid_wikis <- plyr::ldply(covids, data.frame, .id="language")
## covid_wikis$word <- gsub("ENTITY/", "", covid_wikis$word)
## ## covid_wikis <- rbind(
## ##     covid_wikis,
## ##     subset(seeds, language=="ru") %>% mutate(score = NA, word = wikis) %>% select(-wikis)
## ## )
## ##
## covid_all <- plyr::ldply(covids_all, data.frame, .id="language")
## covid_all$word <- gsub("ENTITY/", "", covid_all$word)


theobjs <- c(
    "chinese_leaders_zh_wikis",
    "chinese2013_historical_leaders_zh_wikis",
    ##
    "german_leaders_de_wikis",
    "german2013_historical_leaders_de_wikis",
    ##
    "russian_leaders_ru_wikis",
    "russian2013_historical_leaders_ru_wikis",
    ##
    "iranian2013_historical_leaders_fa_wikis",
    "iranian_leaders_fa_wikis",
    ##
    "italian2013_historical_leaders_it_wikis",
    "italian_leaders_it_wikis",
    ##
    ## "covid_wikis",
    ## "covid_all",
    ##
    names(language_map)
)

save(
    list = theobjs,
    file="zh_fa_it_de_ru_similar_wikipedia_pages_pnas_check.RData"
)
