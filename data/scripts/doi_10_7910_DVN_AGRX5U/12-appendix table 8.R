library(tidyverse)
library(magrittr)
library(stargazer)

"elusive-tab 6.csv" %>%
  read_csv %>% 
  mutate(
    corr = corr %>% 
      factor(corr %>%
               factor %>% 
               levels %>% 
               rev)
  ) %>% 
  dlply(
    .(item),
    function(i)
      lm(ans ~ ideol * corr, i)
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