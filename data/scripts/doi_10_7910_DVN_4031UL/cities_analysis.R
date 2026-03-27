library(tidyverse)
library(xtable)

dat = read.csv("Cities_Final_Results.csv")
head(dat)
dat = dat[order(dat$hitl_score, decreasing=T),]

dat$cut = cut(dat$hitl_score, 15)

set.seed(1000)
samp = dat %>% group_by(cut) %>%
  sample_n(1) %>% arrange(desc(hitl_score)) %>%
  select(amicus, bonica, hitl_score)
samp = samp[,-1]

writeLines(print(xtable::xtable(samp), include.rownames=FALSE), "tableb1.tex")


dat$cut2 = cut(dat$jaccard, 15)

samp = dat %>% group_by(cut2) %>%
  sample_n(1) %>% arrange(desc(jaccard)) %>%
  select(amicus, bonica, jaccard)
samp = samp[,-1]

writeLines(print(xtable::xtable(samp), include.rownames=FALSE), "tableb2.tex")

## In-text figures, supplemental appendix
sum(dat$jaccard > 0.95)
sum(dat$jaccard > 0.90)

sum(dat$hitl_score > 0.95)
sum(dat$hitl_score > 0.90)


dat %>% filter(jaccard > 0.95)
dat %>% filter(hitl_score > 0.95)
