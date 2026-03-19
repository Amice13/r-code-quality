library(plyr)
library(dplyr)
library(tidyverse)
library(broom)
library(here)

setwd(here())
setwd("./wikipedia")

options(stringsAsFactors = F)

languages <- c("fa","ru","zh","de","it")

rel_dates <- list()
byday_agg_all <- list()
byday_agg_all_combined <- list()
tidy_mod_list <- list()
tidy_mod_list_leaders <- list()

rel_dates[["zh"]] <- c("2020-01-24", "2020-03-13")
rel_dates[["fa"]] <- c("2020-03-20","2020-04-18")
rel_dates[["ru"]] <- c("2020-03-28","2020-05-12") #non-working period
rel_dates[["de"]] <- c("2020-03-22","2020-05-06")
rel_dates[["it"]] <- c("2020-03-09","2020-05-18")

language_list2 <- c(
    "zh" = "chinese",
    "it" = "italian",
    "ru" = "russian",
    "fa" = "persian",
    "de" = "german"
)

pstEval <- function(x) {
    eval(parse(text = paste0(x)))
}

leaders_select_vector <- c("TOP","100","250","500","1000")

for (leaders_select in leaders_select_vector) {

    print(leaders_select)

    for (language in languages) {



        print(language)

        byday <- read.table(
            paste0(
                "data/agg_files/",
                sub("_blocked", "", language),
                "_wikipedia_page_views_blocked_v_not_leaders_v_not_2019-12-01_to_2020-09-24_pnas_check",
                ".csv"
            ),
            header=T
        )

        byday <- byday %>%
            dplyr::select(-blocked, -leaders, -historical2013_leaders) %>% as_tibble() # to, as tibble fixes row sums below

        byday <- byday %>%
            mutate(
                leaders = eval(parse(text = paste0("leaders", leaders_select))),
                historical_leaders = eval(parse(text = paste0("historical2013_leaders", leaders_select))),
                blocked = eval(parse(text = paste0("blocked", leaders_select)))
            )
        if (grepl("blocked", language)) {
            byday$historical_leaders <- byday$blocked
        }

        byday_agg <- byday %>%
            filter(date >= "2019-12-01") %>%
            group_by(date) %>%
            summarise(
                all_count = sum(sum_count * as.numeric(
                                                blocked==0 & leaders==0 & historical_leaders == 0
                                            )),
                blocked_count = sum(sum_count * as.numeric(
                                                    blocked==1
                                                )),
                hist_count = sum(sum_count * as.numeric(
                                                 historical_leaders==1
                                             )),
                leaders_count = sum(sum_count * as.numeric(
                                                    leaders==1
                                                ))
            )

        byday_agg_all[[language]] <- byday_agg


    }

    mods <- list()
    mods_leaders <- list()
    for (language in languages) {
        subit <- subset(
            byday_agg_all[[language]],
            (date >= "2019-12-01" & date < "2020-01-01") |
            (date >= as.Date(rel_dates[[language]][1]) & date < as.Date(rel_dates[[language]][1])+30)
        ) %>%
            mutate(lockdown = I(date >= rel_dates[[language]][1] & date < rel_dates[[language]][2]))
        subit <- rbind(
            subit %>% mutate(outcome = hist_count) %>% mutate(sub_or_all="sub"),
            subit %>% mutate(outcome = all_count) %>% mutate(sub_or_all="all")
        )
        subit_leaders <- rbind(
            subit %>% mutate(outcome = leaders_count) %>% mutate(sub_or_all="sub"),
            subit %>% mutate(outcome = all_count) %>% mutate(sub_or_all="all")
        )
        mods[[language]]  <- MASS::glm.nb(
                                       outcome ~ lockdown * I(sub_or_all=="sub"),
                                       data = subit
                                   )
        mods_leaders[[language]]  <- MASS::glm.nb(
                                               outcome ~ lockdown * I(sub_or_all=="sub"),
                                               data = subit_leaders
                                           )
    }

    tidy_mod_list[[leaders_select]] <- mods %>%
        map(
            ~data.frame(
    est=exp(coef(.))[4],
    ci_lower=t(exp(confint(.))[4,1]),
    ci_upper=t(exp(confint(.))[4,2])
)
) %>% ldply(data.frame) %>% rename(language = .id) %>%
    bind_cols(
        data.frame(
            view_count=t(byday_agg_all %>% map_dfr(~sum(.x$hist_count))),
            avg_view_count_pre_lockdown=t(byday_agg_all %>% map(~filter(., date >= "2019-12-01" & date < "2020-01-01")) %>% map_dfr(~mean(.x$hist_count))),
            avg_view_count_lockdown=t(byday_agg_all %>% map(~filter(., date >= rel_dates[[language]][1] & date < rel_dates[[language]][2])) %>% map_dfr(~mean(.x$hist_count)))
        )
    )

    tidy_mod_list_leaders[[leaders_select]] <- mods_leaders %>%
        map(
            ~data.frame(
    est=exp(coef(.))[4],
    ci_lower=t(exp(confint(.))[4,1]),
    ci_upper=t(exp(confint(.))[4,2])
)
) %>% ldply(data.frame) %>% rename(language = .id) %>%
    bind_cols(
        data.frame(
            view_count=t(byday_agg_all %>% map_dfr(~sum(.x$leaders_count))),
            avg_view_count_pre_lockdown=t(byday_agg_all %>% map(~filter(., date >= "2019-12-01" & date < "2020-01-01")) %>% map_dfr(~mean(.x$leaders_count))),
            avg_view_count_lockdown=t(byday_agg_all %>% map(~filter(., date >= rel_dates[[language]][1] & date < rel_dates[[language]][2])) %>% map_dfr(~mean(.x$leaders_count)))
        )
    )

    byday_agg_all_combined[[leaders_select]] <- byday_agg_all

}

tidy_mod_df <- tidy_mod_list %>%
    ldply(data.frame)

tidy_mod_leaders_df <- tidy_mod_list_leaders %>%
    ldply(data.frame)


g1 <- ggplot(
    data=tidy_mod_df %>%
        mutate(.id = factor(.id, levels=leaders_select_vector))
) + geom_pointrange(
        aes(
            group=.id, y=est, ymin=ci_lower, ymax=ci_upper, x=str_to_title(language_list2[language]),
            color=factor(paste(.id=="TOP", grepl("blocked|diss", language)))
        ), position=position_dodge(width=0.5)) +
    geom_hline(yintercept=1, lty=2) + scale_y_continuous(trans="log") +
    theme_classic() +
    theme(axis.text = element_text(size=14), axis.title = element_text(size=14)) +
    xlab("") + ylab("Increase in Views of Historical Leader Pages\n(compared to overall increase in views)") +
    ggtitle("Relative increase in historical leader views:\nfirst 30 days of lockdown") +
    annotate("text", x=4.5, label=c("Leader set order:\n\nn most similar pages - 100\nnot related to    250\ncurrent leader    500\n1,000"), y=1.4, hjust=1,size=4) +
    annotate("text", x=4.5, label=c("atop(bold(Seeds))"), y=1.5, hjust=1,size=4, color="orange", parse=T) +
    scale_color_manual(values=c("TRUE TRUE"="red", "TRUE FALSE" = "orange","FALSE FALSE"="black", "FALSE TRUE"="black")) +
    guides(color=F)
##
g2 <- ggplot(
    data=tidy_mod_df %>%
        mutate(.id = factor(.id, levels=leaders_select_vector))
) + geom_point(
        aes(group=.id, y=view_count, x=str_to_title(language_list2[language]), color=factor(.id=="TOP")), position=position_dodge(width=0.5), size=2) +
    scale_y_continuous(trans="log10", labels = scales::unit_format(unit = "M", scale = 1e-6)) +
    theme_classic() +
    theme(axis.text = element_text(size=14), axis.title = element_text(size=14)) +
    xlab("") + ylab("Number of Historical Leader Page Views") +
    ggtitle("Number of Historical Leader Page Views by Page Set") +
    annotate("text", x=seq(-2, 2, 1)/8+1, label=c("Seed pages","100","250","500", "Top 1k pages"), y=(subset(tidy_mod_df, language=="zh")$view_count*1.15), cex=2.75, color=c("orange", rep("black", 4))) +
    scale_color_manual(values=c("TRUE"="orange","FALSE"="black")) + guides(color=F)
##


g3 <- ggplot(
    data=tidy_mod_leaders_df %>%
        mutate(.id = factor(.id, levels=leaders_select_vector))
) + geom_pointrange(
        aes(
            group=.id, y=est, ymin=ci_lower, ymax=ci_upper, x=str_to_title(language_list2[language]),
            color=factor(paste(.id=="TOP", grepl("blocked|diss", language)))
        ), position=position_dodge(width=0.5)) +
    geom_hline(yintercept=1, lty=2) + scale_y_continuous(trans="log", breaks=c(0.75, 1, 1.5, 2, 3)) +
    theme_classic() +
    theme(axis.text = element_text(size=14), axis.title = element_text(size=14)) +
    xlab("") + ylab("Increase in Views of Current Leader Pages\n(compared to overall increase in views)") +
    ggtitle("Relative increase in current leader views:\nfirst 30 days of lockdown") +
    annotate("text", x=4.7, label=c("Leader set order:\n\nn most similar pages - 100\nnot related to    250\nhistorical leaders    500\n1,000"), y=2.2, hjust=1,size=4) +
    annotate("text", x=4.7, label=c("atop(bold(Seeds))"), y=2.55, hjust=1,size=4, color="purple", parse=T) +
    scale_color_manual(values=c("TRUE TRUE"="red", "TRUE FALSE" = "purple","FALSE FALSE"="black", "FALSE TRUE"="black")) +
    guides(color=F)
##
g4 <- ggplot(
    data=tidy_mod_leaders_df %>%
        mutate(.id = factor(.id, levels=leaders_select_vector))
) + geom_point(
        aes(group=.id, y=view_count, x=str_to_title(language_list2[language]), color=factor(.id=="TOP")), position=position_dodge(width=0.5), size=2) +
    scale_y_continuous(trans="log10", labels = scales::unit_format(unit = "M", scale = 1e-6)) +
    theme_classic() +
    theme(axis.text = element_text(size=14), axis.title = element_text(size=14)) +
    xlab("") + ylab("Number of Current Leader Page Views") +
    ggtitle("Number of Current Leader Page Views by Page Set") +
    annotate("text", x=seq(-2, 2, 1)/8+1, label=c("Seed pages","100","250","500", "Top 1k pages"), y=(subset(tidy_mod_leaders_df, language=="zh")$view_count*1.15), cex=2.75, color=c("purple", rep("black", 4))) +
    scale_color_manual(values=c("TRUE"="purple","FALSE"="black")) + guides(color=F)
##

pdf(paste0("figs/fa_ru_zh_de_it_histleaders_and_currentleaders_robustness_check_test.pdf"), height=4, width=8)
print(g1)
print(g2)
print(g3)
print(g4)

par(mfrow=c(1,1), mar=c(5, 6, 4, 2), xpd=F)
for (language in c("zh","de","it","ru")) {
    theagg <- byday_agg_all[[language]]
    with(
        theagg,
        plot(
            as.Date(date), (log(leaders_count) - log(all_count)) - mean((log(leaders_count) - log(all_count))[date >= "2019-12-01" & date < "2020-01-01"]),
            type="l", col="purple", bty='n',
            main=gsub("\n", " ", str_to_title(language_list2[[language]])),
            ylab="Ratio of historical leader views to\nall Wikipedia page views",
            xlab="Date", ylim=c(-0.25, 1.5), yaxt="n", cex.lab=1.3, cex.main=1.3
        )
    )
    legend("topright", legend=c("Current leaders -\n1k expanded set of pages\n", "Historical leaders -\n1k expanded set of pages\n"), col=c("purple","orange"), lwd=c(1,3), bty="n", cex=0.95)
    theticks <- c(0.5, 1, 2, 4)
    axis(2, at=log(theticks), labels=theticks)
    ##
    with(
        theagg,
        lines(
            as.Date(date), (log(hist_count) - log(all_count)) - mean((log(hist_count) - log(all_count))[date >= "2019-12-01" & date < "2020-01-01"]),
            type="l", col="orange", lwd=3
        )
    )
    abline(v=as.Date(rel_dates[[language]]), col=gray(0.7), lwd=6)
    abline(h=0)
    mtext(text="Lockdown", col=gray(0.5), side=3, at=mean(as.Date(rel_dates[[language]])), cex=1, padj=-0.75)
}

dev.off()
