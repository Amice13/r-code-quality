library(plyr)
library(tidyverse)
library(magrittr)

"elusive-tab 5.csv" %>% 
  read_csv %>% 
  {
    inp <- .
    
    
    j <- 1e3 %>%
      rdply(function()
        {
        wmd_long %>% 
          sample_n(977, T) %$% 
          lm(res ~ ideol*cor*ans_type) %>%  
          predict(newdata = expand.grid(ideol = seq(1, 7,
                                                    length.out = 200),
                                        ans_type = c("item_short",
                                                     "item_long"),
                                        cor = c("C", "NC"))) %>%
          cbind(
            expand.grid(ideol = seq(1, 7,
                                    length.out = 200),
                        ans_type = c("item_short",
                                     "item_long"),
                        cor = c("C", "NC"))
            , .)
        }, .progress = "time") %>%
      tbl_df
    
    names(j)[5] <- "pred"

    j
    
  } %>% 
  spread(cor, pred) %>% 
  mutate(eff = C - NC) %>% 
  group_by(ideol, ans_type) %>% 
  summarize(lo  = eff %>% 
              quantile(.025),
            mid  = eff %>% 
              quantile(.5),
            hi  = eff %>% 
              quantile(.975)
            ) %>% 
  ddply(.(ans_type),
        function(i){
          
          # i <- res_q %>%
          #   filter(ans_type == "item_short")
          
          i %>%
            ungroup %>% 
            mutate(lo_fit = loess(lo ~ ideol, data = i) %>%
                     use_series(fitted),
                   hi_fit = loess(hi ~ ideol, data = i) %>%
                     use_series(fitted))
          
        }) %>% 
  tbl_df %>% 
  mutate(ans_type = ans_type %>% 
           mapvalues(str_c("item_",
                           c("long",
                             "short")),
                     c('Survey Item: "Immediately before the U.S. invasion, Iraq had an active weapons of mass destruction program, the ability to produce these weapons, and large stockpiles of WMD, but Saddam Hussein was able to hide or destroy these weapons right before U.S. forces arrived".',
                       'Survey Item: "Following the US invasion of Iraq in 2003, US forces did not find weapons of mass destruction".')) %>% 
           factor(c('Survey Item: "Immediately before the U.S. invasion, Iraq had an active weapons of mass destruction program, the ability to produce these weapons, and large stockpiles of WMD, but Saddam Hussein was able to hide or destroy these weapons right before U.S. forces arrived".',
                    'Survey Item: "Following the US invasion of Iraq in 2003, US forces did not find weapons of mass destruction".'))) %>% 
  ggplot() +
  geom_hline(yintercept = 0,
             color = "red",
             linetype = 2) +
  geom_ribbon(aes(x = ideol, ymin = lo_fit, ymax = hi_fit),
              alpha = .3,
              size = .25,
              color = "black") +
  geom_text(aes(x, y, label = label),
            angle = 90,
            hjust = .5,
            size = 3.5,
            data = data.frame(x = .75, 
                              y = -.0125,
                              label = "Adoption|Backfire",
                              ans_type = 'Survey Item: "Immediately before the U.S. invasion, Iraq had an active weapons of mass destruction program, the ability to produce these weapons, and large stockpiles of WMD, but Saddam Hussein was able to hide or destroy these weapons right before U.S. forces arrived".'),
            color = "grey20")+
  geom_line(aes(x = ideol, y = mid),
            linetype = 2) +
  geom_segment(aes(x = x, xend = xend,
                   y = y, yend = yend),
               color = "grey50",
               data = data.frame(x = .75,
                                 xend = .75,
                                 y = -.275,
                                 yend = -.4,
                                 ans_type = 'Survey Item: "Immediately before the U.S. invasion, Iraq had an active weapons of mass destruction program, the ability to produce these weapons, and large stockpiles of WMD, but Saddam Hussein was able to hide or destroy these weapons right before U.S. forces arrived".'),
               size = 1,
               arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  geom_segment(aes(x = x, xend = xend,
                   y = y, yend = yend),
               color = "grey50",
               data = data.frame(x = .75,
                                 xend = .75,
                                 y = .275 - .03,
                                 yend = .4 - .03,
                                 ans_type = 'Survey Item: "Immediately before the U.S. invasion, Iraq had an active weapons of mass destruction program, the ability to produce these weapons, and large stockpiles of WMD, but Saddam Hussein was able to hide or destroy these weapons right before U.S. forces arrived".'),
               size = 1,
               arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  facet_grid(. ~ ans_type, 
             labeller = label_wrap_gen(multi_line = T, 
                                       width = 50),
             scales = "free_x",
             space = "free_x") +
  scale_x_continuous("Ideology",
                     breaks = 1:7,
                     labels = str_c(c("Lib",
                                      "",
                                      "",
                                      "Mod",
                                      "",
                                      "",
                                      "Cons"))) +
  scale_y_continuous("Marginal effect of correction",
                     breaks = seq(.5, -.5, -.25),
                     labels = c(".5", ".25", "0", "-.25", "-.5"))
