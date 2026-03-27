library(tidyverse)
library(magrittr)
library(stargazer)

"elusive-tab 2.csv"  %>%
  read_csv %>%
  mutate(
    corr = corr %>% 
      factor(corr %>%
               factor %>% 
               levels %>% 
               rev)
  ) %>% 
  split(.$item) %>% 
  map(
    function(i)
      lm(ans ~ ideol * corr, i)
  ) %>% 
  stargazer(
    type = "text",
    digits = 2,
    keep.stat =  c("n", "rsq")
  )