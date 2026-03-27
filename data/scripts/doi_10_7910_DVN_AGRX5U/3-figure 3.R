library(mise)
mise(pkgs = T)
library(plyr)
library(tidyverse)
library(magrittr)
library(broom)
library(mise)

"elusive-tab 3.csv" %>%
  read_csv %>% 
  ddply(.(t2, speaker),
        function(i)
        {
          
          lm(
            resp_num ~ corr*ideol, data = i) %>%
            # resp_num ~ corr*poly(ideol, 3), data = i) %>% 
            predict(newdata = expand.grid(ideol = seq(1, 7, length.out = 200),
                                          corr = str_c(1:2, 
                                                       ". ", 
                                                       c("Correction",
                                                         "No Correction"))),
                    se = T) %>% 
            cbind(expand.grid(ideol = seq(1, 7, length.out = 200),
                              corr = str_c(1:2, 
                                           ". ", 
                                           c("Correction",
                                             "No Correction"))))
          }
        ) %>% 
  mutate(lo = fit %>% 
           subtract(se.fit %>% 
                      multiply_by(1.96)),
         hi = fit %>% 
           add(se.fit %>% 
                 multiply_by(1.96))) %>% 
                 {
                   
                   inp <- .
                   
                   inp$t2 <-inp$t2 %>% 
                     factor(inp %>% 
                              ddply(.(t2),
                                    function(i)
                                    {
                                      
                                      lm(fit ~ ideol, data = i) %>% 
                                        tidy
                                      
                                      
                                    }) %>% 
                              filter(term == "ideol") %>% 
                              arrange(desc(estimate)) %>% 
                              use_series(t2))
                   
                   
                   inp
                   
                 } %>%
                 {
                  
                   inp <- .
                    
                   inp %>% 
                   ggplot() +
                     geom_ribbon(aes(ideol, ymin = lo, ymax = hi, fill = corr),
                                 alpha = .5,
                                 color = "black",
                                 size = .4) +
                     geom_text(aes(x = ideol,
                                   y = y,
                                   label = label),
                               hjust = 0,
                               angle = 25,
                               data = data.frame(label = "Uncorrected",
                                                 y = 3.9,
                                                 ideol = 1.25,
                                                 speaker = "Liberal Speaker",
                                                 t2 = inp$t2 %>%
                                                   levels %>%
                                                   extract(1)),
                               color = "grey80") +
                     geom_text(aes(x = ideol,
                                   y = y,
                                   label = label),
                               hjust = 0,
                               angle = 11.5,
                               data = data.frame(label = "Corrected",
                                                 y = 2.3,
                                                 speaker = "Liberal Speaker",
                                                 ideol = 1.8,
                                                 t2 = inp$t2 %>%
                                                   levels %>%
                                                   extract(1)),
                               color = "grey40") +
                     scale_fill_grey("",
                                     start = .4, end = .8) +
                     scale_y_continuous("Agreement\n(Larger Values indicate stronger agreement)",
                                        limits = c(1, 5)) +
                     scale_x_continuous("Ideology",
                                        breaks  = seq(1, 7, length.out = 5),
                                        labels = c("L",
                                                   "",
                                                   "M",
                                                   "",
                                                   "C")) +
                     facet_grid(speaker ~ t2,
                                labeller = label_wrap_gen(width = 15))  
                   
                   
                 }