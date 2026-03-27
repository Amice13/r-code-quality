library(tidyverse)
library(magrittr)
library(stargazer)

"elusive-tab 3.csv" %>%
  read_csv %>% 
  mutate(
    corr = corr %>% 
      factor(corr %>%
               factor %>% 
               levels %>% 
               rev)
  ) %>% 
  dlply(
    .(t2, speaker),
    function(i)
      lm(resp_num ~ ideol * corr, i)
  ) %>% 
  stargazer(
    covariate.labels = c(
      "ideol",
      "corr",
      "ideol*corr",
      "cons"
    ),
    type = "text",
    digits = 1,
    keep.stat =  c("n", "rsq")
  )