library(tidyverse)
library(magrittr)
library(stargazer)

"elusive-tab 8.csv" %>%
  read_csv %>% 
  mutate(
    type = type %>% 
      factor(type %>%
               factor %>% 
               levels %>% 
               rev)
  ) %>% 
  dlply(
    .(issue_lab, sample),
    function(i)
      lm(ans_num ~ ideo_num * type, i)
  ) %>% 
  stargazer(
    covariate.labels = c(
      "ideol",
      "corr",
      "ideol*corr",
      "cons"
    ),
    type = "text",
    digits = 2,
    keep.stat =  c("n", "rsq")
  )