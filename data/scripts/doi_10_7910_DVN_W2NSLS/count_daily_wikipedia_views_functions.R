readAndAggDailyPagecounts <- function(
                                .language,
                                .date,
                                .project_variants=c(".z",".m"),
                                .blocked_df=NULL,
                                .leaders_df=NULL,
                                .historical2013_leaders_df=NULL,
                                .covid_df=NULL
                                ) {
    projects <- paste0(.language, .project_variants)
    files <- paste0(
        "~/data/wikipedia/pageviews_daily/",
        projects,
        "-daily-pagecounts-ez-merged_",
        .date, ".txt.gz"
    )
        if (.date == floor_date(as.Date(.date), "month")) {
            cat(
                "    ", .date,
                paste0("(", as.character(Sys.time()), ")"),
                "\n"
            )
        }
    dfs <- list()
    for (i in 1:length(files)) {
        dfs[[projects[i]]] <- read_delim(
                files[i],
                col_names = c("page","daily_count"),
            skip = 1,
                col_types = readr::cols(
                                       page = readr::col_character(),
                                       daily_count = readr::col_integer()
                                   ),
                quote = "",
                comment = "",
                quoted_na = F,
                na = character(),
                delim="\t"
        )
        dfs[[projects[[i]]]]$zm <- .project_variants[i]
    }
    df <- plyr::ldply(dfs, data.frame, .id="project")
    ##
    df$page <- gsub("\\\\", "\"", gsub("\"", "", df$page))
    ##
    if (!is.null(.blocked_df)) {
        BLOCKED <- .blocked_df$leader_name
        BLOCKED_TOP <- subset(
            .blocked_df,
            is_top_leader==1 &
            !(leader_name %in% c("Wikipedia","Wikipedia:首页"))
            )$leader_name
        BLOCKED_NOT_TOP <- subset(
            .blocked_df,
            is_top_leader==0
            )$leader_name
        df$blocked <- as.integer(df$page %in% unique(BLOCKED))
        df$blockedTOP <- as.integer(df$page %in% unique(BLOCKED_TOP))
        if (sum(.blocked_df$is_top_leader==0) > 0) {
        df$blocked100 <- as.integer(df$page %in% c(BLOCKED_TOP, unique(BLOCKED_NOT_TOP)[1:100]))
        df$blocked250 <- as.integer(df$page %in% c(BLOCKED_TOP, unique(BLOCKED_NOT_TOP)[1:250]))
        df$blocked500 <- as.integer(df$page %in% c(BLOCKED_TOP, unique(BLOCKED_NOT_TOP)[1:500]))
        df$blocked1000 <- as.integer(df$page %in% c(BLOCKED_TOP, unique(BLOCKED_NOT_TOP)[1:1000]))
        } else {
        df$blocked100 <- 0
        df$blocked250 <- 0
        df$blocked500 <- 0
        df$blocked1000 <- 0
            }
    } else {
        df$blocked <- 0
        df$blockedTOP <- 0
        df$blocked100 <- 0
        df$blocked250 <- 0
        df$blocked500 <- 0
        df$blocked1000 <- 0
    }
    if (!is.null(.historical2013_leaders_df)) {
        HISTORICAL2013_LEADERS <- .historical2013_leaders_df$leader_name
        HISTORICAL2013_LEADERS_TOP <- subset(
            .historical2013_leaders_df,
            is_top_leader==1
            )$leader_name
        df$historical2013_leaders <- as.integer(df$page %in% unique(HISTORICAL2013_LEADERS))
        df$historical2013_leadersTOP <- as.integer(df$page %in% unique(HISTORICAL2013_LEADERS_TOP))
        if (sum(.historical2013_leaders_df$is_top_leader) > 0) {
        df$historical2013_leaders100 <- as.integer(df$page %in% unique(HISTORICAL2013_LEADERS)[1:100])
        df$historical2013_leaders250 <- as.integer(df$page %in% unique(HISTORICAL2013_LEADERS)[1:250])
        df$historical2013_leaders500 <- as.integer(df$page %in% unique(HISTORICAL2013_LEADERS)[1:500])
        df$historical2013_leaders1000 <- as.integer(df$page %in% unique(HISTORICAL2013_LEADERS)[1:1000])
        } else {
        df$historical2013_leaders100 <- 0
        df$historical2013_leaders250 <- 0
        df$historical2013_leaders500 <- 0
        df$historical2013_leaders1000 <- 0
            }
    } else {
        df$historical2013_leaders <- 0
        df$historical2013_leaders100 <- 0
        df$historical2013_leaders250 <- 0
        df$historical2013_leaders500 <- 0
        df$historical2013_leaders1000 <- 0
    }
    if (!is.null(.leaders_df)) {
        LEADERS <- .leaders_df$leader_name
        LEADERS_TOP <- subset(
            .leaders_df,
            is_top_leader==1
            )$leader_name
        df$leaders <- as.integer(df$page %in% unique(LEADERS))
        df$leadersTOP <- as.integer(df$page %in% unique(LEADERS_TOP))
        if (sum(.leaders_df$is_top_leader) > 0) {
        df$leaders100 <- as.integer(df$page %in% unique(LEADERS)[1:100])
        df$leaders250 <- as.integer(df$page %in% unique(LEADERS)[1:250])
        df$leaders500 <- as.integer(df$page %in% unique(LEADERS)[1:500])
        df$leaders1000 <- as.integer(df$page %in% unique(LEADERS)[1:1000])
        }  else {
        df$leaders100 <- NA
        df$leaders250 <- NA
        df$leaders500 <- NA
        df$leaders1000 <- NA
            }
    } else {
        df$leaders <- 0
        df$leaders100 <- 0
        df$leaders250 <- 0
        df$leaders500 <- 0
        df$leaders1000 <- 0
    }
    if (!is.null(.covid_df) & nrow(.covid_df) != 0) {
        COVID_SEED <- .covid_df$wiki
        df <- df %>%
            mutate(
                covidSEED = as.integer(page %in% unique(COVID_SEED))
            )
    } else {
        df$covidSEED <- 0
    }
    df$date <- .date
    byday <- df %>%
        group_by(
            date, zm,
            leaders, leadersTOP, leaders100, leaders250, leaders500, leaders1000,
            blocked, blockedTOP, blocked100, blocked250, blocked500, blocked1000,
            historical2013_leaders, historical2013_leadersTOP, historical2013_leaders100, historical2013_leaders250, historical2013_leaders500, historical2013_leaders1000,
            covidSEED
        ) %>%
        summarize(sum_count = sum(daily_count))
    ## rm(df)
    return(
        byday
    )
}

getBlockedCounts <- function(
                             .language,
                             .date,
                             .project_variants=c(".z",".m"),
                             .blocked=NULL,
                             .blocked_df=NULL
                             ) {
    projects <- paste0(.language, .project_variants)
    files <- paste0(
        "~/data/wikipedia/pageviews_daily/",
        projects,
        "-daily-pagecounts-ez-merged_",
        .date, ".txt.gz"
    )
        if (.date == floor_date(as.Date(.date), "month")) {
            cat(
                "    ", .date,
                paste0("(", as.character(Sys.time()), ")"),
                "\n"
            )
        }
    if (is.null(.blocked) & !is.null(.blocked_df)) {
        .blocked <- unique(.blocked_df$leader_name)
        }
    thefilter2 <- function(x, pos) {
        x %>%
            mutate(
                page = factor(
                    gsub("\\\\", "\"", gsub("\"", "", page)),
                    levels=c(.blocked, "rest of wikipedia")
                )
            ) %>%
            replace_na(list(page = "rest of wikipedia")) %>%
            group_by(page) %>% summarise(daily_count = sum(daily_count)) %>%
            ungroup()
    }
    dfs <- list()
    for (i in 1:length(files)) {
        dfs[[projects[i]]] <- read_delim_chunked(
                files[i],
                col_names = c("page","daily_count"),
            callback = readr::DataFrameCallback$new(thefilter2), #processed here
        chunk_size = 100000,
                skip = 1,
                col_types = readr::cols(
                                       page = readr::col_character(),
                                       daily_count = readr::col_integer()
                                   ),
                quote = "",
                comment = "",
                quoted_na = F,
                na = character(),
                delim="\t"
        )
    }
    df <- plyr::ldply(dfs, data.frame, .id="project")
    ##
    df$page <- gsub("\\\\", "\"", gsub("\"", "", df$page))
    ##
    df$date <- .date
    ## cat("\n    Aggregating..\n")
    blocked_counts <- df %>%
        group_by(
            date, page
        ) %>%
        summarize(sum_count = sum(daily_count))
    if (!is.null(.blocked_df)) {
        BLOCKED_TOP <- subset(
            .blocked_df,
            is_top_leader==1 &
            !(leader_name %in% c("Wikipedia","Wikipedia:首页"))
            )$leader_name
        BLOCKED_NOT_TOP <- subset(
            .blocked_df,
            is_top_leader==0
        )$leader_name
        blocked_counts <- blocked_counts %>%
            mutate(
        blocked = as.integer(page %in% (BLOCKED)),
        blockedTOP = as.integer(page %in% (BLOCKED_TOP)),
        blocked100 = as.integer(page %in% c(BLOCKED_TOP, (BLOCKED_NOT_TOP)[1:100])),
        blocked250 = as.integer(page %in% c(BLOCKED_TOP, (BLOCKED_NOT_TOP)[1:250])),
        blocked500 = as.integer(page %in% c(BLOCKED_TOP, (BLOCKED_NOT_TOP)[1:500])),
        blocked1000 = as.integer(page %in% c(BLOCKED_TOP, (BLOCKED_NOT_TOP)[1:1000]))
        )
        ##
        blocked_counts <- blocked_counts %>%
            group_by(
                date, blocked, blockedTOP, blocked100, blocked250, blocked500, blocked1000
            ) %>%
            summarise(
                sum_count = sum(sum_count)
            )
    }
    return(
        blocked_counts
    )
}
