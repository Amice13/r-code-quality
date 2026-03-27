library(plyr)
library(dplyr)
library(xtable)
library(here)

setwd(here())
setwd("./wikipedia")

options(stringsAsFactors = F)

languages <- c("fa","ru","zh","de","it")

ma <- function(x, n = 7){stats::filter(x, rep(1 / n, n), sides = 2)}

for_est_overall_increase <- list()
for_est_blocked_increase <- list()
for_est_leaders_increase <- list()
for_est_hist_leaders_increase <- list()

rel_dates <- list()

pdf(paste0("./figs/fa_ru_zh_de_it_byday_topleaders_histleaders_ratio_to_all_test.pdf"), height = 6, width = 12)
par(mfrow=c(1,1), mar = c(5, 6, 8, 3))
for (language in languages) {

    byday <- read.table(
        paste0(
            "data/agg_files/",
            language,
            "_wikipedia_page_views_blocked_v_not_leaders_v_not_2019-12-01_to_2020-09-24_pnas_check.csv"
        ),
        header=T
    )

    for (i in 1:2) {

        if (i==2) {
            byday <- subset(byday, date < "2020-07-01")
            byday_z <- subset(byday, zm==".z")
            byday_m <- subset(byday, zm==".m")
        }

        all <- subset(
            byday,
            blockedTOP == 0 & leadersTOP == 0 & historical2013_leadersTOP == 0
        ) %>%
            group_by(date) %>%
            summarise(sum_count = log(sum(sum_count)))
        if (i==2) {
            all_m <- subset(
                byday_m,
                blockedTOP == 0 & leadersTOP == 0 & historical2013_leadersTOP == 0
            ) %>%
                group_by(date) %>%
                summarise(sum_count = log(sum(sum_count)))
            all_z <- subset(
                byday_z,
                blockedTOP == 0 & leadersTOP == 0 & historical2013_leadersTOP == 0
            ) %>%
                group_by(date) %>%
                summarise(sum_count = log(sum(sum_count)))
        }
        ##
        blocked <- subset(
            byday, blockedTOP == 1
        ) %>%
            group_by(date) %>%
            summarise(sum_count = log(sum(sum_count)))
        ##
        hist_leaders <- subset(
            byday, historical2013_leadersTOP == 1
        ) %>%
            group_by(date) %>%
            summarise(sum_count = log(sum(sum_count)))
        leaders <- subset(
            byday, leadersTOP == 1
        ) %>%
            group_by(date) %>%
            summarise(sum_count = log(sum(sum_count)))

        if (i==1) {
            for_est_overall_increase[[language]] <- all[,c("date", "sum_count")]
            for_est_blocked_increase[[language]] <- blocked[,c("date", "sum_count")]
            for_est_leaders_increase[[language]] <- leaders[,c("date", "sum_count")]
            for_est_hist_leaders_increase[[language]] <- hist_leaders[,c("date", "sum_count")]
        }

    }


    mean_pre_all <- mean(
        subset(all, as.Date(date) < "2020-01-01")$sum_count
    )
    mean_pre_all_m <- mean(
        subset(all_m, as.Date(date) < "2020-01-01")$sum_count
    )
    mean_pre_all_z <- mean(
        subset(all_z, as.Date(date) < "2020-01-01")$sum_count
    )
    mean_pre_leaders <- mean(
        subset(leaders, as.Date(date) < "2020-01-01")$sum_count
    )
    mean_pre_blocked <- mean(
        subset(blocked, as.Date(date) < "2020-01-01")$sum_count, na.rm=T
    )
    mean_pre_histleaders <- mean(
        subset(hist_leaders, as.Date(date) < "2020-01-01")$sum_count, na.rm=T
    )

    plot(
        as.Date(all_m$date),
        (all$sum_count - mean_pre_all),
        type="l", lwd=4, bty="n", xlab="Date", ylab="Total Views\n(Compared to Pre-Covid Level)",
        cex.axis=1.5, cex.lab=1.5,
        main=paste0(toupper(language), ": ","Change in Page"," Views per Day"),
        cex.main=1.5, yaxt="n"
    )
    ##
    theticks <- c(0.9, 1, 1.1, 1.2, 1.3, 1.4, 1.5)
    axis(2, at=log(theticks), labels=theticks)
    abline(h=0, col="black", lwd=4, lty=3)
    if (language == "zh") {
        zh_dates <- c("2020-01-24", "2020-03-13")
        abline(v=as.Date(zh_dates), col=gray(0.7), lwd=6)
        rel_dates[[language]] <- zh_dates
        mtext(text="Lockdown", col=gray(0.7), side=3, at=mean(as.Date(rel_dates[[language]])), cex=1.5)
    }
    if (language == "fa") {
        fa_dates <- c("2020-03-20","2020-04-18")
        abline(v=as.Date(fa_dates), col=gray(0.7), lwd=6)
        rel_dates[[language]] <- fa_dates
        mtext(text="Lockdown", col=gray(0.7), side=3, at=mean(as.Date(rel_dates[[language]])), cex=1.5)
    }
    if (language == "ru") {
        ru_dates <- c("2020-03-28","2020-05-12") #non-working period
        abline(v=as.Date(ru_dates), col=gray(0.7), lwd=6)
        rel_dates[[language]] <- ru_dates
        mtext(text="Lockdown", col=gray(0.7), side=3, at=mean(as.Date(rel_dates[[language]])), cex=1.5)
    }
    if (language == "de") {
        de_dates <- c("2020-03-22","2020-05-06")
        abline(v=as.Date(de_dates), col=gray(0.7), lwd=6)
        rel_dates[[language]] <- de_dates
        mtext(text="Lockdown", col=gray(0.7), side=3, at=mean(as.Date(rel_dates[[language]])), cex=1.5)
    }
    it_dates <- c("2020-03-09","2020-05-18")
    if (language == "it") {
        abline(v=as.Date(it_dates), col=gray(0.7), lwd=6)
        rel_dates[[language]] <- it_dates
        mtext(text="Lockdown", col=gray(0.7), side=3, at=mean(as.Date(rel_dates[[language]])), cex=1.5)
    }


    plot(
        as.Date(all_m$date),
        ma((all_m$sum_count - mean_pre_all_m) - (all_z$sum_count - mean_pre_all_z)),
        type="l", lwd=4, bty="n", xlab="Date", ylab="Total Views\n(Compared to Pre-Covid Level)",
        cex.axis=1.5, cex.lab=1.5,
        main=paste0(toupper(language), ": ","Ratio of Mobile to Desktop"," Views per Day"),
        cex.main=1.5, yaxt="n", col = "blue"
    )
    ##
    theticks <- c(0.9, 1, 1.1, 1.2, 1.3, 1.4, 1.5)
    axis(2, at=log(theticks), labels=theticks)
    abline(h=0, col="black", lwd=4, lty=3)
    if (language == "zh") {
        abline(v=as.Date(zh_dates), col="black", lwd=4)
    }
    if (language == "fa") {
        abline(v=as.Date(fa_dates), col="black", lwd=4)
    }
    if (language == "ru") {
        abline(v=as.Date(ru_dates), col="black", lwd=4)
    }
    if (language == "de") {
        abline(v=as.Date(de_dates), col="black", lwd=4)
    }
    if (language == "it") {
        abline(v=as.Date(it_dates), col="black", lwd=4)
    }


    divide_by <- (mean_pre_all - mean_pre_all)
    divide_by_leaders <- (mean_pre_leaders - mean_pre_all)
    divide_by_histleaders <- (mean_pre_histleaders - mean_pre_all)
    if (language %in% c("fa","zh")) {
        divide_by_blocked <- (mean_pre_blocked - mean_pre_all)
    }

    all$sum_count <- all$sum_count - divide_by
    blocked$sum_count <- (blocked$sum_count - (all$sum_count)) - divide_by_blocked
    leaders$sum_count <- ((leaders$sum_count - divide_by_leaders) - (all$sum_count))
    hist_leaders$sum_count <- ((hist_leaders$sum_count - divide_by_histleaders) - (all$sum_count))
    all$sum_count <- (all$sum_count - all$sum_count)

    if (language %in% c("fa","zh")) {
        all_vecs <- c(
            all$sum_count,
            blocked$sum_count,
            leaders$sum_count,
            hist_leaders$sum_count
        )
        themin <- min(all_vecs)
        themax <- max(all_vecs)
    } else {
        all_vecs <- c(
            all$sum_count,
            leaders$sum_count,
            hist_leaders$sum_count
        )
        themin <- min(all_vecs)
        themax <- max(all_vecs)
    }

    plot(
        as.Date(all$date),
        all$sum_count, type = "l",
        ylim=log(c(0.5, ifelse(language=="it", 8, 6))),
        bty="n",
        lwd = 4,
        xlab = "Date",
        ylab = "Wikipedia Page Views\nRatio to Rest of Wikipedia",
        main = paste0(toupper(language), ": ","Page Views by Category"),
        cex.main=1.5,
        cex.axis=1.5,
        cex.lab=1.5,
        yaxt="n"
    )
    theticks <- c(0.25, 0.5, 1, 2, 4)
    lines(
        as.Date(all_z$date),
        ma((all_m$sum_count - mean_pre_all_m) - (all_z$sum_count - mean_pre_all_z)),
        lwd=4, lty = 1, col = "blue"
    )
    axis(2, at=log(theticks), labels=theticks)
    if (!is.nan(mean_pre_blocked)) {
        lines(
            as.Date(blocked$date),
            blocked$sum_count,
            col = "red",
            lwd = 4
        )
    }
    if (!is.nan(mean_pre_histleaders)) {
        lines(
            as.Date(hist_leaders$date),
            hist_leaders$sum_count,
            col = "orange",
            lwd = 4
        )
    }
    if (!is.null(divide_by_leaders)) {
        lines(
            as.Date(leaders$date),
            leaders$sum_count,
            col = "purple",
            lwd = 4
        )
    }
    lines(
        as.Date(all$date),
        all$sum_count,
        lwd=4
    )

    if (language == "zh") {
        abline(v=as.Date(zh_dates), col=gray(0.7), lwd=6)
        mtext(text="Lockdown", col=gray(0.5), side=3, at=mean(as.Date(zh_dates)), cex=1.5, padj=-1.5)
    }
    if (language == "fa") {
        abline(v=as.Date(fa_dates), col=gray(0.7), lwd=6)
        mtext(text="Lockdown", col=gray(0.5), side=3, at=mean(as.Date(fa_dates)), cex=1.5)
    }
    if (language == "ru") {
        abline(v=as.Date(ru_dates), col=gray(0.7), lwd=6)
        mtext(text="Lockdown", col=gray(0.5), side=3, at=mean(as.Date(ru_dates)), cex=1.5)
    }
    if (language == "de") {
        abline(v=as.Date(de_dates), col=gray(0.7), lwd=6)
        mtext(text="Lockdown", col=gray(0.5), side=3, at=mean(as.Date(de_dates)), cex=1.5)
    }
    if (language == "it") {
        abline(v=as.Date(it_dates), col=gray(0.7), lwd=6)
        mtext(text="Lockdown", col=gray(0.5), side=3, at=mean(as.Date(it_dates)), cex=1.5)
    }
    if (language %in% c("de","ru")) {
        abline(v=as.Date("2020-03-09"), col=gray(0.7), lty=3, lwd=3)
        mtext(text="Lockdown in Italy", side=3, at=as.Date(it_dates)[1])
    }

    if (language %in% c("fa","zh")) {
        legend(
            ifelse(language%in% c("fa"), "topleft", "topright"),
            legend = c(
                "Current Leader Pages",
                "Historical Leader Pages",
                "Blocked Pages (pre-https)",
                "Rest of Wikipedia (=1)",
                "Ratio of Mobile to Desktop Views (7-day MA)"
            ),
            col = c("purple","orange","red","black","blue"),
            lty = 1,
            lwd = 4,
            bty="n",
            cex=1.2
        )
    } else {
        legend(
            ifelse(language%in% c("it","de","ru"), "topleft", "topright"),
            legend = c(
                "Current Leader Pages",
                "Historical Leader Pages",
                "Rest of Wikipedia (=1)",
                "Ratio of Mobile to Desktop Views (7-day MA)"## ,## ,
            ),
            col = c("purple","orange","black","blue"),
            lty = 1,
            lwd = 4,
            bty="n",
            cex=1.2
        )
    }
    rm(blocked)## ; rm(covid)
}

dev.off()


mods <- list()
mods2 <- list()
mods3 <- list()
for (which_pages in c("overall","blocked","leaders","hist_leaders")) {
    for (language in c("zh","de","it","fa","ru")) {
        if (which_pages=="blocked" & !(language %in% c("zh","fa"))) next
        ##
        this_for_est <- get(paste0("for_est_", which_pages, "_increase"))
        ##
        if (!(which_pages == "overall")) {
            ##
            this_for_est[[language]] <- rbind(
                this_for_est[[language]] %>% mutate(sub_or_all="sub"),
                subset(
                    for_est_overall_increase[[language]],
                    date < rel_dates[[language]][2]
                    & !(
                        date > "2020-01-01"
                        & date < rel_dates[[language]][1]
                    )
                ) %>% mutate(sub_or_all="all")
            )
            ## )
            print(language)
            ##
            mods[[paste(which_pages, language, sep="_")]] <- MASS::glm.nb(
                exp(sum_count[date > "2019-12-01"]) ~ I((date >= rel_dates[[language]][1])[date > "2019-12-01"]) * I(sub_or_all[date > "2019-12-01"]=="sub"),
                data = subset(
                    this_for_est[[language]],
                    date < rel_dates[[language]][2]
                    & !(
                        date > "2020-01-01"
                        & date < rel_dates[[language]][1]
                    )
                )
            )
        } else {
            mods[[paste(which_pages, language, sep="_")]] <- MASS::glm.nb(
                exp(sum_count[date > "2019-12-01"]) ~ I((date >= rel_dates[[language]][1])[date > "2019-12-01"]),
                data = subset(
                    this_for_est[[language]],
                    date < rel_dates[[language]][2]
                    & !(
                        date > "2020-01-01"
                        & date < rel_dates[[language]][1]
                    )
                )
            )
        }
    }
}

getCoefs <- function(i, .mods=mods) {
    thelength <- length(coef(.mods[[i]]))
    rbind(
        round(exp(coef(.mods[[i]])[thelength]), 2),
        paste0(
            "\\small{(",
            paste(round(exp(confint(.mods[[i]])[thelength,]), 2), collapse=" - "),
            ")}"
        ),
        round(coef(summary(.mods[[i]]))[thelength,4], 5)
    )
}

combineCoefs <- function(js, .mods=mods) {
    do.call(
        "rbind",
        lapply(
            js,
            function(i)
                getCoefs(i, .mods=.mods)
        )
    )
}

tableCoefs <- function(.mods=mods) {
    allCoefs <- data.frame(
        combineCoefs(js=1:5, .mods=.mods),
        c(combineCoefs(js=6:7, .mods=.mods), rep("", 9)),
        combineCoefs(js=8:12, .mods=.mods),
        combineCoefs(js=13:17, .mods=.mods)
    )
    names(allCoefs) <- unique(sapply(strsplit(names(.mods), "_"), "[", 1))
    for_rownames <- unique(
        sapply(
            strsplit(names(.mods), "_"), "[", 2
        )
    )
    rownames(allCoefs) <- paste0(
        rep(for_rownames[-(which(for_rownames=="leaders"))], each=3), rep(c("","_ci","_p"), 5)
    )
    return(allCoefs)
}

xtable(
    tableCoefs(.mods=mods)
)
