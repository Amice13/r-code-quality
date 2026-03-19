library(tidyverse)
library(xtable)

dat = read.csv("Survey_Final_Results.csv")
dat$amicus = gsub(" \\([RD]\\)", "", dat$amicus)
dat$bonica = gsub(" \\([RD]\\)", "", dat$bonica)
colnames(dat)[6:8] = c("incumbent", "votedfor", "confidence")

set.seed(1)

dat$cut = cut(dat$confidence, 22)

samp = dat %>% group_by(cut) %>%
  sample_n(1) %>% arrange(desc(confidence)) %>%
  select(incumbent, votedfor, confidence)
samp = samp[,-1]

writeLines(print(xtable(samp), include.rownames=FALSE), "tableb3.tex")

dat$cut2 = cut(dat$jaccard, 22)
samp = dat %>% group_by(cut2) %>%
  sample_n(1) %>% arrange(desc(jaccard)) %>%
  select(incumbent, votedfor, jaccard)
samp = samp[,-1]

## Not included in the manuscript
print(xtable(samp), include.rownames=FALSE)

## In-text figures, supplemental appendix
sum(dat$AFSM_score > 0.21 & dat$label == 0)
sum(dat$AFSM_score > 0.21)

sum(dat$jaccard > 0.8)
sum(dat$jaccard > 0.8 & dat$label == 0)
