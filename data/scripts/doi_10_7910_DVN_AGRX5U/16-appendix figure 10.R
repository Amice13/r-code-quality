library(plyr)
library(tidyverse)
library(stringr)
library(magrittr)
library(broom)

str_c("ans ~ ",
      c("co",
        "cumcorr",
        "max_seq"),
      "*corr*ideol") %>% 
  ldply(function(i)
    "elusive-tab 11.csv" %>% 
      read_csv %>% 
      ddply(
        .(item,
          study),
        possibly(
          function(j)
            lm(i %>%
                 as.formula,
               data = j) %>%
            tidy,
          NULL
        )
      ) %>% 
      mutate(covar = i %>%
               str_sub(
                 6,
                 str_locate(
                   i,
                   fixed("*"))[, 1] %>%
                   subtract(1)
               )
      )
  ) %>% 
  tbl_df %>% 
  select(item, study, covar, everything()) %>% 
  filter(term %>% 
           is_in(c("co",
                   "cumcorr",
                   "max_seq")) | 
           term %>% 
           str_detect(c("co",
                        "cumcorr",
                        "max_seq") %>% 
                        str_c(":") %>% 
                        str_c(collapse = "|"))) %>%  
  mutate(t2 = str_c("b", 
                    1:4,
                    "\n") %>% 
           str_c(c("Survey Quantity",
                   "Correction x Survey Quantity",
                   "Correction x Ideology",
                   "Correction x Ideology x Survey Quantity")) %>% 
           rep(times = 150) %>% 
           factor(c(str_c("b", 
                          1:4,
                          "\n") %>% 
                      str_c(c("Survey Quantity",
                              "Correction x Survey Quantity",
                              "Correction x Ideology",
                              "Correction x Ideology x Survey Quantity")))),
         signif = p.value %>% 
           is_weakly_less_than(.05) %>% 
           as.numeric
  ) %>% 
  {
    inp <- .
    
    inp %>% 
      left_join(
        inp %>% 
          select(item, study) %>% 
          group_by(item, study) %>% 
          slice(1) %>% 
          ungroup %>% 
          mutate(lg = 1:n()))
  } %>% 
  mutate(
    covar = covar %>% 
      mapvalues(
        covar %>% 
          unique,
        c("Correction Order",
          "Cumulative Corrections",
          "Maximum Sequence of Successive Corrections")
      ) %>% 
      factor(
        c("Correction Order",
          "Cumulative Corrections",
          "Maximum Sequence of Successive Corrections"))
  ) %>% 
  {
    inp <- .
    
    inp %>% 
      ggplot() +
      geom_rect(xmin = 0,
                xmax = 4.5,
                ymin = 1.96,
                ymax  = 3.7,
                color = "grey90",
                fill = "grey90") + 
      geom_rect(xmin = 0,
                xmax = 4.5,
                ymax = -1.96,
                ymin  = -3.7,
                color = "grey90",
                fill = "grey90") + 
      geom_text(aes(x, 
                    y,
                    label = lab),
                size = 2,
                data =  data.frame(x = 1.25,
                                   y = 3.5,
                                   lab = "Region of Statistical Significance",
                                   covar = inp$covar %>% 
                                     levels %>%
                                     extract(1))) +
      geom_line(aes(t2 %>% 
                      as.numeric, 
                    statistic,
                    group = lg),
                alpha = .2) +
      geom_point(aes(t2 %>% 
                       as.numeric, 
                     statistic, 
                     shape = signif %>% 
                       factor,
                     size = signif %>% 
                       factor),
                 alpha = .6) +
      facet_grid(covar ~ .,
                 labeller = label_wrap_gen(width = 15)) +
      scale_x_continuous(breaks = 1:4,
                         labels = ml$t2 %>%
                           levels %>% 
                           str_replace_all("y x Survey Quantity",
                                           "y x\nSurvey Quantity")) +
      scale_y_continuous(expand = c(0, 0),
                         limits = c(-3.7, 
                                    3.7)) +
      scale_size_manual(values = c(2.5, 3.5)) +
      scale_shape_manual(values = c(16, 1) %>% 
                           rev) + 
    labs(x = "Linear Model Term", 
         y = "Linear Model Coefficient t value")
  }
    
