library(arsenal)
rm(list=ls())
gc()
source('replication/12_nycc_prep.R')
resp_d %>%
    drop_na(map_time) %>%
    mutate(usable = map_int(neighborhood, length) > 1) %>%
ggplot(aes((0.1 + map_time)/60, usable)) +
    geom_boxplot() +
    scale_x_continuous("Time spent drawing map (min)", trans="log10") +
    scale_y_discrete(name=NULL, labels=c("Empty neigborhood", "Usable neigborhood")) +
    theme_paper()
ggsave(here("paper/figures/map_time_nycc.pdf"), width=6, height=2)

resp_d %>%
    drop_na(map_time) %>%
    mutate(usable = map_int(neighborhood, length) > 1,
           map_time = map_time/60,
           party = droplevels(party_combined)) %>%
    select(map_time, usable, group, city,
           party, age, gender, education, retired, race, homeowner) %>%
    tableby(cut(map_time, breaks=c(0, 1, 2, Inf)) ~ ., data=., total=F) %>%
    padjust(method="holm") %>%
    summary(pfootnote=T) %>%
    knitr::kable(format="markdown",
                 caption="Descriptive statistics by total time spent drawing neighborhood (minutes)") %>%
    writeLines(here("paper/tables/map-time-nycc.md"))
