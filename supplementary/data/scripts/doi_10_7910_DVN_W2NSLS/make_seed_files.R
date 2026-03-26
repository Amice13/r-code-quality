library(dplyr)
library(here)

setwd(here())
setwd("./wikipedia/data/ref_files")

options(stringsAsFactors=F)

formatWriteLeaders <- function(nationality, language, entities_only=TRUE) {
    top_leaders <- gsub(" ", "", gsub(
        paste0("https://",language,".wikipedia.org/wiki/"), "",
        read.csv(
            paste0(nationality, "_leader_names_and_wiki_pages_checked_february.csv"),
            comment.char="#"
        )$native_wiki
    )
        )
    ##
    if (FALSE) {
    leaders <- read.csv(
        wiki2vecfile
    )$wiki
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
    leaders_df <- subset(leaders_df, !is.na(leader_name) & leader_name !="NA")
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
    write.csv(
        leaders_df,
        file=paste0("current_", nationality, "_leaders_wiki_seeds_formatted_pnas_check.csv")
    )
    return(leaders_df)
}

formatWriteHistoricalLeaders <- function(nationality, language, entities_only=TRUE) {
    top_leaders <- gsub(" ", "", gsub(
        paste0("https://",language,".wikipedia.org/wiki/"), "",
        subset(read.csv(
            paste0("historical_", nationality, "_leaders_wiki_seeds.csv"),
            comment.char="#"
        ), current == 0)$wikis
    )
        )
    ##
    if (FALSE) {
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
    leaders_df <- subset(leaders_df, !is.na(leader_name) & leader_name !="NA")
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
    write.csv(
        leaders_df,
        file=paste0("historical_", nationality, "_leaders_wiki_seeds_formatted_pnas_check.csv")
    )
    return(leaders_df)
}


iranian_leaders_fa_wikis <- formatWriteLeaders("iranian",language="fa")
chinese_leaders_zh_wikis <- formatWriteLeaders("chinese",language="zh")
chinese_leaders_zh_all <- formatWriteLeaders("chinese", entities_only=FALSE,language="zh")
italian_leaders_it_wikis <- formatWriteLeaders("italian",language="it")
german_leaders_de_wikis <- formatWriteLeaders("german",language="de")
russian_leaders_ru_wikis <- formatWriteLeaders("russian",language="ru")

iranian2013_historical_leaders_fa_wikis <- formatWriteHistoricalLeaders(
    "iranian2013","fa"
)
chinese2013_historical_leaders_zh_wikis <- formatWriteHistoricalLeaders(
    "chinese2013","zh"
)
korean2013_historical_leaders_ko_wikis <- formatWriteHistoricalLeaders(
    "korean2013","ko"
)
italian2013_historical_leaders_it_wikis <- formatWriteHistoricalLeaders(
    "italian2013","it"
    )
german2013_historical_leaders_de_wikis <- formatWriteHistoricalLeaders(
    "german2013","de"
)
russian2013_historical_leaders_ru_wikis <- formatWriteHistoricalLeaders(
    "russian2013","ru"
)

combineForWiki2vec <- function(nationality, negative_nationality, language) {
    leaders1 <- get(paste0(nationality, "_leaders_",language,"_wikis"))
    leaders2 <- get(paste0(negative_nationality, "_leaders_",language,"_wikis"))
    get_vector <- if (language %in% c("de","it")) {
                      2
                  } else if (language=="fa") {
                      1:2
                      } else {1} # ru, zh
    if (!grepl("historical", negative_nationality)) {
        leaders2 <- leaders2[get_vector,]
    }
    combined <- rbind(
        leaders1 %>%
            select(leader_name) %>% mutate(negative_case = 0),
        leaders2 %>%
        select(leader_name) %>% mutate(negative_case = 1)
    )
    write.csv(
        combined,
        file=paste0(nationality, "_leaders_wiki_seeds_combined_for_wiki2vec_pnas_check.csv")
    )
    return(combined)
}

combineForWiki2vec("iranian", "iranian2013_historical", "fa")
combineForWiki2vec("iranian2013_historical", "iranian", "fa")
combineForWiki2vec("chinese", "chinese2013_historical", "zh")
combineForWiki2vec("chinese2013_historical", "chinese", "zh")
combineForWiki2vec("german", "german2013_historical", "de")
combineForWiki2vec("german2013_historical", "german", "de")
combineForWiki2vec("italian", "italian2013_historical", "it")
combineForWiki2vec("italian2013_historical", "italian", "it")

combineForWiki2vec("russian", "russian2013_historical", "ru")
combineForWiki2vec("russian2013_historical", "russian", "ru")




seeds <- read.csv("covid_sars_wiki_seeds_with_pandemic.csv")
seeds$wikis <- sapply(
        gsub(
            "https://[a-z][a-z].wikipedia.org/wiki/", "",
            seeds$wikis
        ),
    URLdecode
)


formatCovidWords <- function(.language, data=seeds) {
    myseeds <- subset(seeds, language==.language)
    myseeds$leader_name <- myseeds$wikis
    myseeds$negative_case <- 0
    write.csv(
        myseeds,
        file=paste0(.language, "_covid_wiki_seeds_combined_for_wiki2vec_pnas_check.csv")
    )
    }
formatCovidWords("zh")
formatCovidWords("ru")
formatCovidWords("de")
formatCovidWords("it")
formatCovidWords("fa")
