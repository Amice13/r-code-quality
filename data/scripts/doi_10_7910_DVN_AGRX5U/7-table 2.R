library(plyr)
library(tidyverse)
library(stringr)
library(magrittr)

d1 <- "elusive-tab 7.csv" %>% 
  read_csv

d1$age %<>% 
  factor(
    d1$age %>% 
      unique %>% 
      sort
  )

d1$education %<>%
  factor(d1$education %>%
           unique %>% 
           extract(
             c(2, 3, 1, 4)
             )
  )

d1$race %<>% 
  factor(
    d1$race %>%
      unique %>%
      extract(-5)
  )

d1$gender %<>% 
  factor(
    c("Male",
      "Female")
  )

d1$income %<>% 
  factor(
    d1$income %>% 
      unique %>% 
      extract(
        c(1, 3, 2, 4)
      )
  )

d1$region %<>% 
  factor(d1$region %>%
           unique %>% 
          extract(
            c(2, 3, 1, 4) 
          )
  )

d1$employment %<>% 
  factor(
    d1$employment %>%
      unique %>% 
      extract(c(-2))
  )

d1$ideology %<>% 
  factor(d1$ideology %>%
           unique %>% 
           extract(
             c(3, 1, 2)
            )
  )

d1$sample %<>% 
  factor(
    c("Lucid",
      "Turk")
  )

d1 %>% 
  gather(cat, vars, -c(sample, resp),
         na.rm  = T) %>% 
  mutate(
    cat = cat %>% 
      factor(
        d1 %>% 
          names %>% 
          extract(1:8)
        ),
    vars = vars %>% 
      factor(
        d1 %>% 
          select(1:8) %>% 
          map(~levels(.)) %>% 
          unlist %>% 
          as.character
      )
  ) %>% 
  group_by(sample, cat, vars) %>% 
  tally %>% 
  mutate(prop = n %>% 
           divide_by(
             n %>% 
               sum
             ) %>% 
           multiply_by(100) %>% 
           round
         ) %>% 
  select(-n) %>% 
  arrange(cat, vars) %>% 
  ungroup %>% 
  unite(cv, c(cat, vars), sep = "_") %>% 
  spread(cv, prop)