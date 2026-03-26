library(rjson)
library(dplyr)
library(here)

options(stringsAsFactors=F)

#### this script was used to quickly find wikipedia pages for other languages.
#### pages used in later analyses were checked by hand

setwd(here())
setwd("./wikipedia/data/ref_files")

source(here("code/get_leader_in_other_languages_functions.R"))
## getLeaderFromEN, getWikisFromName

## see get_leaders_in_fa.R for Iran
iranian_leaders <- read.csv(
    "iranian_leaders.csv",
    comment.char="#"
)
iranian_leaders$name <- rep(NA, nrow(iranian_leaders))

chinese_leaders <- read.csv(
    "chinese_leaders.csv",
    comment.char="#"
)
chinese_leader_df <- getWikisFromName(
    input_df = chinese_leaders,
    source_lang = "zh"
)
write.csv(
    chinese_leader_df,
    file="chinese_leader_names_and_wiki_pages.csv",
    row.names=F
)

italian_leaders <- read.csv(
    "italian_leaders.csv",
    comment.char="#"
)
italian_leaders$name <- rep(NA, nrow(italian_leaders))
italian_leader_df <- getLeaderFromEN(
    input_df = italian_leaders,
    target_lang = "it"
)
write.csv(
    italian_leader_df %>%
    mutate(correct = "", corrected_link = "") %>%
    select(-cia_name.name) %>% rename(cia_name = cia_name.en_name),
    file="italian_leader_names_and_wiki_pages_CANDIDATE_MATCHES.csv",
    row.names=F
)

german_leaders <- read.csv(
    "german_leaders.csv",
    comment.char="#"
)
german_leaders$name <- rep(NA, nrow(german_leaders))
german_leader_df <- getLeaderFromEN(
    input_df = german_leaders,
    target_lang = "de"
)
write.csv(
    german_leader_df %>%
    mutate(correct = "", corrected_link = "") %>%
    select(-cia_name.name) %>% rename(cia_name = cia_name.en_name),
    file="german_leader_names_and_wiki_pages_CANDIDATE_MATCHES.csv",
    row.names=F
)

russian_leaders <- read.csv(
    "russian_leaders.csv",
    comment.char="#"
)
russian_leaders$name <- rep(NA, nrow(russian_leaders))
russian_leader_df <- getLeaderFromEN(
    input_df = russian_leaders,
    target_lang = "ru"
)
write.csv(
    russian_leader_df %>%
    mutate(correct = "", corrected_link = "") %>%
    select(-cia_name.name) %>% rename(cia_name = cia_name.en_name),
    file="russian_leader_names_and_wiki_pages_CANDIDATE_MATCHES.csv",
    row.names=F
)
