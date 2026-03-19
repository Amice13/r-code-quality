library(plyr)
library(tidyverse)
library(magrittr)
library(lme4)

"elusive-tab 8.csv" %>% 
  read_csv %>% 
  {
   
    inp <- .
    
    lm_dat <- expand.grid(
      ideo_num = seq(1, 5, length.out = 100),
      type = c("No Correction",
               "Correction"),
      sample = c("Lucid",
                 "Turk")
      ) %>%
      tbl_df
    
    lp <- rdply(400,
          lm_dat %>%
            mutate(fit = lmer(ans_num ~ type * ideo_num * sample + (1 + ideo_num | issue_lab),
                              data = inp %>%
                                sample_frac(.6, replace = T)
                              ) %>%
                     predict(re.form = NA, newdata = lm_dat)),
                .progress = "time") %>% 
      tbl_df 
    
    
    lp2 <- lp %>% 
      tbl_df %>% 
      spread(type, fit) %>% 
      mutate(mu = Correction - `No Correction`) %>% 
      group_by(ideo_num, sample) %>% 
      summarize(lo = mu %>% 
                  quantile(.025),
                mid = mu %>% 
                  quantile(.5),
                hi = mu %>% 
                  quantile(.975)) %>% 
      ungroup %>% 
      rbind.fill(lp %>%
                   tbl_df %>% 
                   spread(type, fit) %>% 
                   mutate(mu =  Correction -`No Correction`) %>%
                   select(.n:sample, mu) %>% 
                   spread(sample, mu) %>% 
                   mutate(mu2 =  Turk - Lucid) %>%
                   group_by(ideo_num) %>% 
                   summarize(lo = mu2 %>% 
                               quantile(.025),
                             mid = mu2 %>% 
                               quantile(.5),
                             hi = mu2 %>% 
                               quantile(.975)) %>%
                   mutate(sample = "Difference")
                 ) %>% 
      mutate(
        sample = sample %>% 
          mapvalues(
            sample %>%
            unique,
          c("Lucid Sample",
            "MTurk Sample",
            "MTurk Sample - Lucid Sample")
          ) %>%
          factor(
            c("MTurk Sample",
              "Lucid Sample",
              "MTurk Sample - Lucid Sample")
            )
      )  %>% 
      ddply(.(sample),
            function(i){
              i %>% 
                mutate(fit_lo = loess(lo ~ ideo_num, span = .75, data = i) %>%
                         fitted.values,
                       fit_hi = loess(hi ~ ideo_num, span = .75, data = i) %>%
                         fitted.values )
              }
            )
    
    
    lp2_bub <- lp2 %>% 
      group_by(sample) %>%
      slice(c(1, 50, 100)) %>% 
      mutate(ideo_num = c(1, 3, 5),
             lab = mid %>% 
               round(2) %>% 
               as.character %>% 
               str_replace("0.", ".") %>% 
               str_pad(width = 4, "right", 0))
    
    lp2 %>% 
      ggplot() +
      geom_hline(yintercept = 0, linetype  = 3) +
      geom_ribbon(aes(ideo_num, mid, ymin = fit_lo, ymax = fit_hi),
                  fill = "grey85",
                  alpha = .5,
                  color = "black", size = .35) +
      geom_line(aes(ideo_num, mid), color = "black", linetype = 2,
                data = lp2_bub) +
      geom_point(aes(ideo_num, mid), shape = 1, color = "black", size = 9,
                 data = lp2_bub) +
      geom_point(aes(ideo_num, mid), shape = 16, color = "grey98", size = 8.7,
                 data = lp2_bub) +
      geom_text(aes(ideo_num, mid, label = lab),
                size = 3,
                data = lp2_bub) +
      facet_wrap(~sample, nrow = 1) +
      scale_y_continuous(breaks = seq(0, -.8, -.2),
                         labels = c("0", "-.2", 
                                    "-.4", "-.6", "-.8")) +
      scale_x_continuous(expand = c(.09, .09),
                         breaks = c(1, 3, 5),
                         labels = c("Lib",
                                    "Mod",
                                    "Cons")) +
      guides(fill = F) +
      labs(x = "Ideology",
           y = "Correction effects--difference on 5pt scale\n(Negative values indicate greater factual adoption)",
           color = "",
           title = "Correction effects by sample",
           subtitle = "Quantities taken from multilevel model.")
    }