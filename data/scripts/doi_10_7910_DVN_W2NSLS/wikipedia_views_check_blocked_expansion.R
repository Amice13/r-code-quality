library(plyr)
library(dplyr)
library(tidyverse)
library(broom)
library(here)

setwd(here())
setwd("./wikipedia")

options(stringsAsFactors = F)

languages <- c("fa","ru","zh","de","it")

load("data/agg_files/all_blocked_page_counts_2019_to_2020.RData")

rel_dates <- list()
byday_agg_all <- list()
byday_agg_all_combined <- list()
tidy_mod_list <- list()
tidy_mod_list_leaders <- list()

rel_dates[["zh"]] <- c("2020-01-24", "2020-03-13")
rel_dates[["zh_blocked"]] <- rel_dates[["zh"]]
rel_dates[["fa"]] <- c("2020-03-20","2020-04-18")
rel_dates[["fa_blocked"]] <- rel_dates[["fa"]]
rel_dates[["ru"]] <- c("2020-03-28","2020-05-12")
rel_dates[["ru_diss"]] <- rel_dates[["ru"]]
rel_dates[["de"]] <- c("2020-03-22","2020-05-06")
rel_dates[["it"]] <- c("2020-03-09","2020-05-18")

language_map <- c(
    "zh_blocked_expanded" = "zh",
    "fa_blocked_expanded" = "fa",
    "fa_blocked_expanded_no_filter" = "fa",
    "fa_significant_increases" = "fa",
    "ru_significant_increases_add_bbc_mentions" = "ru",
    "russian_dissidents_navalny_only_no_putin_diff" = "ru"
)

language_list2 <- c(
    "zh_blocked_expanded" = "Chinese\nblocked in China:\npre-https",
    "fa_blocked_expanded" = "Persian\nblocked in Iran:\npre-https, political",
    "fa_blocked_expanded_no_filter" = "Persian\nblocked in Iran:\npre-https",
    "fa_significant_increases" = "Persian\nblocked in Iran:\nbiggest \nincrease seeds",
    "ru_significant_increases_add_bbc_mentions" = "Russian\nbiggest\nincrease seeds",
    "russian_dissidents_navalny_only_no_putin_diff" = "Russian\nAlexei Navalny"
)


pstEval <- function(x) {
    eval(parse(text = paste0(x)))
}

leaders_select_vector <- c("TOP","100","250","500","1000")

for (leaders_select in leaders_select_vector) {

    print(leaders_select)

    for (thedf in names(language_list2)) {



        thiscol <- paste0("blocked", leaders_select)

        byday <- all_blocked_counts[[thedf]] %>% ungroup() %>%
            mutate(blocked10k = blocked) %>% dplyr::select(-blocked)

        if (leaders_select!="TOP") {
            all_blocked_cols <- grep("blocked", names(byday), value=T)
            byday$blocked_higher <- rowSums(byday[,all_blocked_cols[1:(which(all_blocked_cols==thiscol)-1)]])
            ##
            byday <- byday %>%
                filter(blockedTOP == 0) # don't let seed pages swamp effects
        }

        byday <- byday %>%
            mutate(
                blocked = eval(parse(text = thiscol))
            )

        byday_agg <- byday %>%
            filter(date >= "2019-12-01") %>%
            group_by(date) %>%
            summarise(
                all_count = sum(sum_count * as.numeric(
                                                blocked==0
                                            )),
                blocked_count = sum(sum_count * as.numeric(
                                                    blocked==1
                                                ))
                )

        byday_agg_all[[thedf]] <- byday_agg


    }

    mods <- list()
    mods_leaders <- list()
    for (thedf in names(language_list2)) {
        subit <- subset(
            byday_agg_all[[thedf]],
            (date >= "2019-12-01" & date < "2020-01-01") |
            (date >= as.Date(rel_dates[[language_map[thedf]]][1]) & date < as.Date(rel_dates[[language_map[thedf]]][1])+30)
        ) %>%
            mutate(lockdown = I(date >= rel_dates[[language_map[thedf]]][1] & date < rel_dates[[language_map[thedf]]][2]))
        subit <- rbind(
            subit %>% mutate(outcome = blocked_count) %>% mutate(sub_or_all="sub"),
            subit %>% mutate(outcome = all_count) %>% mutate(sub_or_all="all")
        )
        mods[[thedf]]  <- MASS::glm.nb(
                                       outcome ~ lockdown * I(sub_or_all=="sub"),
                                       data = subit
                                   )
    }

    tidy_mod_list[[leaders_select]] <- mods %>%
        map(
            ~data.frame(
    est=exp(coef(.))[4],
    ci_lower=t(exp(confint(.))[4,1]),
    ci_upper=t(exp(confint(.))[4,2])
)
) %>% ldply(data.frame) %>% rename(thedf = .id) %>%
    bind_cols(
        data.frame(
            view_count=t(byday_agg_all %>% map_dfr(~sum((.x$blocked_count)))),
            avg_view_count_pre_lockdown=t(byday_agg_all %>% map(~filter(., date >= "2019-12-01" & date < "2020-01-01")) %>% map_dfr(~mean(.x$blocked_count))),
            avg_view_count_lockdown=t(byday_agg_all %>% map(~filter(., date >= rel_dates[[language_map[thedf]]][1] & date < rel_dates[[language_map[thedf]]][2])) %>% map_dfr(~mean(.x$blocked_count)))
        )
    )

        byday_agg_all_combined[[leaders_select]] <- byday_agg_all

}

tidy_mod_df <- tidy_mod_list %>%
    ldply(data.frame)

theticks <- c(0.25, 0.5, 0.75, 1, 1.33, 1.5, 2, 4)

g1 <- ggplot(
    data=tidy_mod_df %>%
        filter(.id!="TOP" | thedf %in% c("zh_blocked_expanded","fa_blocked_expanded","fa_blocked_expanded_no_filter","russian_dissidents_navalny_only_no_putin_diff")) %>%
        mutate(.id = factor(.id, levels=leaders_select_vector))
) + geom_pointrange(
        aes(
            group=.id, y=est, ymin=ci_lower, ymax=ci_upper, x=str_to_title(language_list2[thedf]),
            color=factor(.id=="TOP")
        ), position=position_dodge(width=0.5)) +
    geom_hline(yintercept=1, lty=2) +
    geom_vline(xintercept=c(1.5, 4.5), lty=3) +
    theme_classic() +
    theme(axis.text = element_text(size=12), axis.title = element_text(size=12)) +
    xlab("") + ylab("Increase in Views of \"Sensitive\" Political Pages\n(compared to overall increase in views)") +
    ggtitle("Relative increase in \"sensitive\" political page views:\nfirst 30 days of lockdown") +
    annotate("text", x=3.1, label=c("Leader set order:\n\nn most similar pages - 100\n    250\n    500\n1,000"), y=1.41, hjust=1,size=3.25) +
    annotate("text", x=3.1, label=c("Seeds"), y=1.675, hjust=1,size=3.25, color="red", parse=T) +
    scale_color_manual(values=c("TRUE"="red","FALSE"="black")) +
    guides(color=F) +
    scale_y_continuous(
        trans="log",
        breaks=theticks,
        labels=format(theticks, digits=2)
    )
##
g2 <- ggplot(
    data=tidy_mod_df %>%
        mutate(.id = factor(.id, levels=leaders_select_vector))
) + geom_point(
        aes(group=.id, y=view_count, x=str_to_title(language_list2[thedf]), color=factor(.id=="TOP")), position=position_dodge(width=0.5), size=2) +
    scale_y_continuous(trans="log10", labels = scales::unit_format(unit = "M", scale = 1e-6)) +
    theme_classic() +
    theme(axis.text = element_text(size=12), axis.title = element_text(size=12)) +
    xlab("") + ylab("Number of Sensitive Political Page Views") +
    ggtitle("Number of Sensitive Political Page Views by Page Set") +
    annotate("text", x=seq(-2, 2, 1)/8+1, label=c("Seed pages","100","250","500", "Top 1k pages"), y=(subset(tidy_mod_df, thedf=="zh_blocked_expanded")$view_count*1.5), cex=2.75, color=c("red", rep("black", 4))) +
    scale_color_manual(values=c("TRUE"="red","FALSE"="black")) +
    guides(color=F)
##



load("data/agg_files/ru_page_views_2019_to_2020_explore_politics_add_documentary_slogan_and_bbc_mentions.RData")
ru_by_page <- all_list_counts %>% ungroup() %>%
    filter(date >= "2019-12-01")

ru_page_labels <- read.csv("data/ref_files/russian_opposition_media_human_rights_wiki_seeds_add_documentary_slogan_and_bbc_mentions.csv")

ru_page_labels$russian_name <- sapply(gsub("https://ru.wikipedia.org/wiki/", "", ru_page_labels$name), URLdecode)

ru_by_page <- left_join(ru_by_page, ru_page_labels %>% select(english_name, russian_name), by=c("page" = "russian_name"))

added_pages <- c(
    "https://en.wikipedia.org/wiki/Anti-Corruption_Foundation" = "\n\nAnti-Corruption\nFoundation",
    "https://en.wikipedia.org/wiki/Russia_of_the_Future" = "Russia of\nthe Future",
    "https://en.wikipedia.org/wiki/2017%E2%80%932018_Russian_protests" = "2017-2018\nRussian protests",
    "https://en.wikipedia.org/wiki/Russian_Opposition_Coordination_Council" = "Russian\nOpposition\nCoordination\nCouncil",
    "https://en.wikipedia.org/wiki/2019_Moscow_protests" = "2019\nMoscow protests",
    "https://en.wikipedia.org/wiki/2011%E2%80%932013_Russian_protests" = "\n\n2011-2013\nRussian protests",
    "https://en.wikipedia.org/wiki/2018_Russian_pension_protests" = "\n\n2018 Russian\npension protests",
    "https://en.wikipedia.org/wiki/He_Is_Not_Dimon_to_You" = "He Is Not\nDimon to You",
    "https://en.wikipedia.org/wiki/Party_of_crooks_and_thieves" = "\n\nParty of crooks\nand thieves"
    )
navalny_related <- c(
    "https://en.wikipedia.org/wiki/Alexei_Navalny" = "Alexei Navalny",
    added_pages[order(names(added_pages))]
)

ru_by_page <- ru_by_page %>%
    filter(english_name %in% names(navalny_related) | page == "rest of wikipedia")


ru_mods <- list()
byday_agg_ru_opp <- list()
for (thedf in names(navalny_related)) {
    subit <- ru_by_page %>%
        filter(page == "rest of wikipedia" | english_name==thedf) %>%
        group_by(page, date) %>% # combine previous name of 2017-2018 protests
        summarise(sum_count = sum(sum_count)) %>%
        mutate(
            sub_or_all = case_when(
                page=="rest of wikipedia" ~ "all",
                TRUE ~ "sub"
            ),
            outcome=sum_count
        ) %>%
    complete(page, nesting(date), fill=list(sum_count = 0, outcome = 0))
    ##
    byday_agg_ru_opp[[thedf]] <- subset(subit, page!="rest of wikipedia")
    ##
    subit <- subit %>%                  #
        filter(
        (date >= "2019-12-01" & date < "2020-01-01") |
        (date >= as.Date(rel_dates[["ru"]][1]) & date < as.Date(rel_dates[["ru"]][1])+30)
        ) %>%
        mutate(lockdown = I(date >= rel_dates[["ru"]][1] & date < rel_dates[["ru"]][2]))
    ru_mods[[thedf]]  <- MASS::glm.nb(
                                   outcome ~ lockdown * I(sub_or_all=="sub"),
                                   data = subit
                               )
    ##
}

ru_tidy_mods <- ru_mods %>%
    map(
        ~data.frame(
    est=exp(coef(.))[4],
    ci_lower=t(exp(confint(.))[4,1]),
    ci_upper=t(exp(confint(.))[4,2]),
    p_value=coef(summary(.))[4,4]
)
) %>% ldply(data.frame) %>%
    mutate(
        thedf = navalny_related[.id]
    ) %>%
    bind_cols(data.frame(view_count=t(byday_agg_ru_opp %>% map_dfr(~sum((.x$sum_count))))))


navalny_related_levels <- unname(navalny_related)

g3 <- ggplot(
    data=ru_tidy_mods %>%
        mutate(thedf = factor(thedf, levels=navalny_related_levels))
) + geom_pointrange(
        aes(
            group=thedf, y=est, ymin=ci_lower, ymax=ci_upper, x=thedf,
            color=factor(thedf=="Alexei Navalny")
        ), position=position_dodge(width=0.5)) +
    geom_hline(yintercept=1, lty=2) +
    theme_classic() +
    theme(axis.text = element_text(size=12), axis.title = element_text(size=14)) +
    xlab("") + ylab("Increase in Views of Pages Related to Alexei Navalny\n(compared to overall increase in views)") +
    ggtitle("Relative increase in Alexei Navalny related page views (in Russian):\nfirst 30 days of lockdown") +
    scale_color_manual(values=c("TRUE"="red","FALSE"="black")) +
    guides(color=F) +
    scale_y_continuous(
        trans="log",
        breaks=theticks,
        labels=format(theticks, digits=2)
    )

g4 <- ggplot(
    data=ru_tidy_mods %>%
        mutate(thedf = factor(thedf, levels=navalny_related_levels))
) + geom_point(
        aes(
            group=thedf, y=view_count, x=thedf,
            color=factor(thedf=="Alexei Navalny")
        ), position=position_dodge(width=0.5), size=2) +
    theme_classic() +
    theme(axis.text = element_text(size=12), axis.title = element_text(size=14)) +
    xlab("") + ylab("Number of Page Views") +
    ggtitle("Number of Page Views") +
    scale_color_manual(values=c("TRUE"="red","FALSE"="black")) +
    guides(color=F) +
    scale_y_continuous(trans="log10", labels = scales::unit_format(unit = "M", scale = 1e-6))

pdf(paste0("figs/fa_ru_zh_politically_sensitive_pages_robustness_check_test.pdf"), height=4, width=9)
print(g1)
print(g2)
print(g3)
print(g4)
dev.off()


checked_ru_pages <- read.csv("data/ref_files/russian_opposition_media_human_rights_wiki_seeds_add_documentary_slogan_and_bbc_mentions.csv")

checked_ru_pages$english_name <- gsub("%E2%80%93", "-", gsub("https://en.wikipedia.org/wiki/", "", gsub("_", " ", checked_ru_pages$english_name)))

checked_ru_pages <- checked_ru_pages %>% arrange(english_name)

library(xtable)
print(
    xtable(
        data.frame(
            unique(checked_ru_pages$english_name)[1:ceiling(length(unique(unique(checked_ru_pages$english_name)))/2)],
            c(unique(checked_ru_pages$english_name)[(ceiling(length(unique(unique(checked_ru_pages$english_name)))/2)+1):length(unique(unique(checked_ru_pages$english_name)))])
            )
    ),
    include.rownames=F
)

## used for bolding names in table (by hand)
sig_increases <- read.csv(
    "data/ref_files/ru_significant_increases_add_bbc_mentions_wiki_seeds_combined_for_wiki2vec_pnas_check.csv"
)

inner_join(
    checked_ru_pages %>% mutate(name = sub("https://ru.wikipedia.org/wiki/", "", name)),
    sig_increases %>% mutate(leader_name = sapply(leader_name, URLencode)),
    by=c("name" = "leader_name")
)
