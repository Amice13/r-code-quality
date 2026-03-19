library(tidyverse)
library(magrittr)
library(lme4)

"elusive-tab 6.csv" %>%
  read_csv %>% 
  mutate(
    corr = corr %>% 
      factor(corr %>%
               factor %>% 
               levels %>% 
               rev)
  ) %$%
  lmer(
    ans ~ ideol*corr*complexity_num*speaker + (1|item),
    data  = d1
    ) %>% 
  summary
