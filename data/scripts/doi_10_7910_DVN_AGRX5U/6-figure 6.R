library(plyr)
library(tidyverse)
library(magrittr)
library(lme4)
library(emmeans)

lmer(ans ~ ideol*corr*complexity_num*speaker +
       (1|item),
     data  = "elusive-tab 6.csv" %>% 
       read_csv
     ) %>% 
  emmeans(
    ~corr|ideol + complexity_num + speaker,
    at = list(
      ideol = seq(1, 7, length.out = 100),
      complexity_num = 1:3,
      speaker = c("Conservative",
                  "Liberal")
              )
    ) %>% 
  pairs %>%
  tidy %>% 
  tbl_df %>% 
  mutate(
    lo = estimate %>%
      subtract(std.error %>%
                 multiply_by(1.96)),
     hi = estimate %>% 
       add(std.error %>%
             multiply_by(1.96)),
     speaker = speaker %>% 
       mapvalues(c("Conservative",
                   "Liberal"),
                 c("Conservative Speaker",
                   "Liberal Speaker")) %>% 
       factor(c("Liberal Speaker",
                "Conservative Speaker")),
     complexity = complexity_num %>% 
       mapvalues(1:3,
                 c("Complex Survey Item",
                   "Moderate Survey Item",
                   "Simple Survey Item") %>% 
                   rev) %>% 
       factor(c("Complex Survey Item",
                "Moderate Survey Item",
                "Simple Survey Item") %>% 
                rev)
    ) %>% 
  ggplot(aes(ideol, ymin = lo, ymax = hi)) +
  geom_hline(yintercept = 0,
             linetype = 2,
             color  = "grey10") +
  geom_ribbon(aes(ideol, ymin = lo, ymax = hi),
              alpha = .5,
              fill = "grey70",
              color = "black",
              size = .4) +
  facet_grid(complexity ~ speaker,
             labeller = label_wrap_gen(width = 15)) + 
  scale_y_continuous("Marginal effect of correction \n(Difference on 5pt scale)") +
  scale_x_continuous("Ideology",
                     breaks  = seq(1, 7, length.out = 5),
                     labels = c("L",
                                "",
                                "M",
                                "",
                                "C"))

  

