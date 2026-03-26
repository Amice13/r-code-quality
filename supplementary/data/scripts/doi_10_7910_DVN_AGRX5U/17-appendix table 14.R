library(plyr)
library(tidyverse)
library(stringr)
library(magrittr)


d1 <- "elusive-tab 12.csv" %>% 
  read_csv

com_res <- d1 %>% 
  filter(type == "Continuous") %>% 
  mutate(val = val %>% as.numeric) %>% 
  group_by(study, cont_val, com_surv) %>% 
  summarize(v2 = cor(val, com_surv_val) %>% 
              round(2) %>% 
              as.character %>% 
              str_replace(fixed("0."), ".")) %>% 
  ungroup %>% 
  mutate(v2 = v2 %>% 
           str_length %>% 
           equals(1) %>% 
           ifelse(v2 %>% 
                    str_replace("0",
                                ".01"),
                  v2)) %>% 
  mutate(type = "Continuous") %>% 
  select(type, study, cont_val, everything()) %>% 
  rbind.fill(d1 %>% 
               filter(type != "Continuous") %>% 
               ddply(.(study, 
                       cont_val,
                       com_surv),
                     possibly(function(i)
                       xtabs("~ val + com_surv_val" %>%
                               as.formula,
                             data = i) %>%
                         cramersV,
                       NA),
                     .progress = "time") %>% 
               mutate(V1 = V1 %>% 
                        round(2) %>% 
                        as.character %>% 
                        str_replace("0.",
                                    ".")) %>% 
               ungroup %>% 
               mutate(type = "Categorical") %>% 
               rename("v2" = V1))

com_res %>% 
  tbl_df %>%
  arrange(type, com_surv, study) %>% 
  unite(css, c(study, com_surv), sep = "_") %>% 
  mutate(css = css %>% 
           factor(css %>% 
                    unique)) %>% 
  spread(css, v2) 
