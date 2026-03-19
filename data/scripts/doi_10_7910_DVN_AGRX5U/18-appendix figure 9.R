library(plyr)
library(tidyverse)
library(stringr)
library(magrittr)
library(broom)

ideol_tab <- expand.grid(ideol = seq(1, 7, length.out = 100),
                         corr = c("Correction",
                                  "No Correction")) %>% 
  tbl_df

party_tab <- expand.grid(partyid = seq(1, 7, length.out = 100),
                         corr = c("Correction",
                                  "No Correction")) %>% 
  tbl_df

"elusive-tab 13.csv" %>%
  read_csv %>% 
  ddply(.(study, 
          item),
        possibly(
          function(i){
            
             250 %>% 
              rdply(function(){
                
                j <- i %>% 
                  sample_frac(1, replace = T)
                
                party_tab %>% 
                  mutate(fit = lm(ans ~ corr * partyid, 
                                  data = j) %>% 
                           predict(newdata = party_tab)) %>% 
                  spread(corr, fit) %>% 
                  mutate(party_diff = `No Correction` - Correction) %>% 
                  select(partyid, party_diff) %>% 
                  mutate(ideol_diff = ideol_tab %>%
                           mutate(fit = lm(ans ~ corr * ideol,
                                           data = j) %>%
                                    predict(newdata = ideol_tab)) %>%
                           spread(corr, fit) %>% 
                           mutate(ideol_diff = `No Correction` - Correction) %>% 
                           use_series(ideol_diff),
                         diff2 = party_diff %>% 
                           subtract(ideol_diff)) %>% 
                  select(partyid, diff2)
              }) %>% 
              # .progress = "time") %>% 
              tbl_df %>% 
              group_by(partyid) %>% 
              summarize(lo = diff2 %>% 
                          quantile(.025),
                        mu = diff2 %>% 
                          quantile(.5),
                        hi = diff2 %>% 
                          quantile(.975))
          },
          NULL),
        .progress = "time") %>% 
        {
          
          inp <- .
          
          inp$item[inp$item %>% 
                    str_detect("(\\d)") %>% 
                    not] %<>% 
            str_c("(5)")
          
          inp
          
          
        }%>% 
  ddply(.(study, item),
        function(i)
      
          data.frame(flo = loess(lo ~ partyid, data = i) %>%
                       use_series(fitted),
                     fmu = loess(mu ~ partyid, data = i) %>% 
                       use_series(fitted),
                     fhi = loess(hi ~ partyid, data = i) %>%
                       use_series(fitted)
          )) %>% 
  tbl_df %>% 
  {
    
    inp <- .
    
    inp$xvar <- seq(1, 7, length.out = 100) %>% 
      rep(times = nrow(inp)/100)
    
    inp$item %<>%
      factor(inp %>%
               ddply(.(item),
                     function(i)
                       lm(fmu ~ xvar, data = i) %>% 
                       tidy) %>%
               filter(term == "xvar") %>% 
               select(item, estimate) %>% 
               arrange(estimate) %>% 
               use_series(item))
    inp
  } %>% 
  group_by(item, xvar) %>% 
  slice(1) %>% 
  ggplot(aes(xvar, ymin = flo, y = fmu, ymax = fhi)) +
  geom_ribbon(fill = "grey90",
              color = "grey20",
              size = .2) +
  geom_hline(yintercept = 0,
             linetype = 2,
             size = .5) +
  facet_wrap(~item, ncol = 4,
             labeller = label_wrap_gen(width = 35)) +
  scale_y_continuous(breaks = c(.6, 0, -.6),
                     labels = c(".6",
                                "0",
                                "-.6")) +
  scale_x_continuous(breaks = c(1.5, 
                                4, 
                                6.5),
                     labels = c("Lib\nDem",
                                "Mod\nInd",
                                "Cons\nRep")) +
  labs(x = "Predictive covariate: Partisanship or Ideology",
       y = "Partisan correction effect - Ideological correction effect") 
