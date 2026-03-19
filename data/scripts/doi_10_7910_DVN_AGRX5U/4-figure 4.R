library(plyr)
library(tidyverse)
library(magrittr)
library(broom)

"elusive-tab 4.csv" %>%
  read_csv %>%
  dlply(
    .(item),
    function(i)
      
      lm(ans ~ corr*ideol,
         data = i)
    
  ) %>%
  ldply(function(i){
    
    j <- i %>% 
      predict(newdata = expand.grid(
        ideol = seq(1, 7, length.out = 100),
        corr = c("Correction",
                 "No Correction") %>% 
          factor),
        se.fit = T)
    
    expand.grid(
      ideol = seq(1, 7, length.out = 100),
      corr = c("Correction",
               "No Correction") %>% 
        factor
    ) %>% 
      mutate(fit = j$fit,
             lo = fit %>% 
               subtract(j$se.fit %>% 
                          multiply_by(1.96)),
             hi = fit %>% 
               add(j$se.fit %>%
                     multiply_by(1.96)))
  }) %>% 
  tbl_df %>% 
  {
    
    inp <- .
    
    inp$item  %<>%
      factor(inp %>%
               ddply(.(item),
                     function(i)
                       lm(fit ~ ideol, data = i) %>%
                       tidy) %>% 
               filter(term == "ideol") %>% 
               arrange(estimate) %>% 
               use_series(item))
    
    inp
    
  }  %>% 
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
                angle = 360-5,
                data = data.frame(label = "Uncorrected",
                                  y = 4.05,
                                  ideol = 2.5,
                                  item = inp$item %>%
                                    levels %>%
                                    extract(1)),
                color = "grey80") +
      geom_text(aes(x = ideol,
                    y = y,
                    label = label),
                hjust = 0,
                angle = 360-5,
                data = data.frame(label = "Corrected",
                                  y = 3.05,
                                  speaker = "Liberal Speaker",
                                  ideol = 2.5,
                                  item = inp$item %>%
                                    levels %>%
                                    extract(1)),
                color = "grey40") +
      scale_fill_grey("",
                      start = .8, end = .4) +
      scale_y_continuous("Agreement\n(Larger Values indicate stronger agreement)",
                         limits = c(1, 5)) +
      scale_x_continuous("Ideology",
                         breaks  = seq(1, 7, length.out = 5),
                         labels = c("L",
                                    "",
                                    "M",
                                    "",
                                    "C")) +
      facet_wrap(~ item,
                 labeller = label_wrap_gen(width = 25),
                 nrow = 2)
  }