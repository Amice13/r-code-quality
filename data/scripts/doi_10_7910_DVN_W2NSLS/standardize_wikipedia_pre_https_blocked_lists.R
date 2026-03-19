library(here)

setwd(here())
setwd("./wikipedia/data/ref_files")

options(stringsAsFactors = F)

## Hobbs and Roberts
load("all_zh_blocked_page_counts_2014.RData")
china <- BLOCKED

china$page <- sapply(china$page_title, URLdecode)

china <- subset(
    china,
    !(page %in% c("Wikipedia:首页", "Wikipedia"))
)

## Nazeri and Anderson "Citation Filtered: Iran’s Censorship of Wikipedia"
##     unlike the Chinese list, it looks like this list is missing the current
##     Iranian leadership
iran <- read.csv(
    "country_IR_wiki_fawiki_date_20130409_unavailable.csv"
    )

##
zh_blocked <- unique(china$page)
zh_blocked <- sapply(zh_blocked, URLdecode) # dont know why needed to run again
fa_blocked <- unique(iran[,1])

## wayy more chinese blocked pages than farsi blocked pages
length(zh_blocked)
length(fa_blocked)

save(
    zh_blocked, fa_blocked,
    file = "zh_fa_pre_https_blocked_wikipedia_pages.RData"
)
