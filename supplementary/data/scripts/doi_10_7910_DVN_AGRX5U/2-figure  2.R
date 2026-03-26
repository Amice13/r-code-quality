library(tidyverse)
library(magrittr)

"elusive-tab 2.csv"  %>%
  read_csv %>% 
  ddply(.(item),
        function(i)
        {      
          pred_obj <- lm(
            ans ~ ideol*corr,
            data = i) %>%
            predict(newdata = expand.grid(
              ideol = seq(1, 7, length.out = 300),
              corr = c("1. Correction",
                       "2. No Correction")
            ),
            se = T)
          
          data.frame(y_hat = pred_obj$fit,
                     y_se = pred_obj$se.fit) %>% 
            cbind(
              expand.grid(
                ideol = seq(1, 7, length.out = 300),
                corr = c("1. Correction",
                         "2. No Correction")
              )
              )
          },
        .progress = "time") %>% 
  mutate(lo = y_hat %>% 
           subtract(y_se %>% 
                      multiply_by(1.96)),
         hi = y_hat %>% 
           add(y_se %>% 
                 multiply_by(1.96))) %>% 
  tbl_df %>% 
  {
    inp <- .
    
    inp$item %<>% 
      str_replace_all("\n", " ")  

    inp$item %<>%
    factor(inp %>%
             ddply(.(item),
                   function(i)
                     lm(y_hat ~ ideol, 
                        data = i) %>%
                     tidy) %>%
             filter(term == "ideol") %>% 
             arrange(estimate) %>% 
             use_series(item)
           )
    
    inp
  } %>% 
  {
    inp2 <- .
    
    inp2 %>%
      ggplot() +
      geom_ribbon(aes(x = ideol, ymin = lo, ymax = hi, fill = corr),
              alpha = .5,
              color = "black",
              size = .5) +
  geom_text(aes(x = ideol,
                y = y,
                label = label),
            hjust = 0,
            angle = 360-22.5,
            data = data.frame(label = "Uncorrected",
                              y = 4,
                              ideol = 3,
                              item = inp2$item %>%
                                levels %>%
                                extract(1)),
            color = "grey80") +
  geom_text(aes(x = ideol,
                y = y,
                label = label),
            hjust = 0,
            angle = 360-20,
            data = data.frame(label = "Corrected",
                              y = 2.8,
                              ideol = 3,
                              item = inp2$item %>%
                                levels %>%
                                extract(1)),
            color = "grey40") +
  facet_wrap( ~ item,
              nrow = 2,
              labeller = label_wrap_gen(width = 25)) +
  scale_fill_grey("",
                  start = .4, end = .8) +
  scale_y_continuous("Agreement\n(Larger Values indicate stronger agreement)",
                     limits = c(1, 5)) +
  scale_x_continuous("Respondent Ideology",
                     breaks  = 1:7,
                     labels = c("Lib",
                                "",
                                "",
                                "Mod",
                                "",
                                "",
                                "Cons")
                     )
  }
  
