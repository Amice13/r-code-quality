library(plyr)
library(dplyr)
library(tidyverse)
library(broom)
library(here)

setwd(here())
setwd("./wikipedia")

options(stringsAsFactors = F)

load("data/agg_files/fa_page_views_2019_to_2020_explore_politics.RData")
fa_by_page <- all_list_counts %>% ungroup()

load("data/agg_files/ru_page_views_2019_to_2020_explore_politics_add_documentary_slogan_and_bbc_mentions.RData")
ru_by_page <- all_list_counts %>% ungroup()


tidy_mod_list_by_page <- list()
for (language in c("fa","ru")) {
    mods_by_page <- list()
    for (thepage in unique(get(paste0(language, "_by_page"))$page)) {
        subit <- subset(
            get(paste0(language, "_by_page")) %>% filter(page==thepage | page=="rest of wikipedia"),
            (date >= "2019-12-01" & date < "2020-01-01") |
            (date >= as.Date(rel_dates[[language]][1]) & date < as.Date(rel_dates[[language]][1])+30)
        ) %>%
            mutate(
                lockdown = I(date >= rel_dates[[language]][1] & date < rel_dates[[language]][2]),
                outcome = sum_count
            ) %>%
            complete(page, nesting(date), fill=list(sum_count = 0, outcome = 0))
        subit <- rbind(
            subit %>% filter(page==thepage) %>% mutate(sub_or_all="sub"),
            subit %>% filter(page=="rest of wikipedia") %>% mutate(sub_or_all="all")
        )
        mods_by_page[[thepage]]  <- MASS::glm.nb(
                                              outcome ~ lockdown * I(sub_or_all=="sub"),
                                              data = subit
                                          )
    }

    tidy_mod_list_by_page[[language]] <- mods_by_page %>%
        map(
            ~data.frame(
    est=exp(coef(.))[4],
    ci_lower=t(exp(confint(.))[4,1]),
    ci_upper=t(exp(confint(.))[4,2]),
    p_value = ifelse(is.na(coef(.)[4]),NA, coef(summary(.))[4,4])
)
) %>% ldply(data.frame) %>% rename(page = .id)
}

write.csv(
    data.frame(
        leader_name=tidy_mod_list_by_page[["ru"]] %>% filter(p_value < 0.05 / nrow(.) & est > 1) %>% pull(page),
        negative_case = 0
    ),
    file="data/ref_files/ru_significant_increases_add_bbc_mentions_wiki_seeds_combined_for_wiki2vec_pnas_check.csv"
)

write.csv(
    data.frame(
        leader_name=tidy_mod_list_by_page[["fa"]] %>%
            filter(p_value < 0.05 / nrow(.) & est > 1) %>%
            pull(page),
        negative_case = 0
    ),
    file="data/ref_files/fa_significant_increases_wiki_seeds_combined_for_wiki2vec_pnas_check.csv"
)
