# summary results table for corrections
library(plyr)
library(tidyverse)
library(magrittr)
library(emmeans)
library(broom)
library(ggstance)

"elusive-tab 1.csv" %>% 
  read_csv %>% 
  mutate(
    corr = corr  %>%
      factor(
        corr %>%
          factor %>%
          levels %>%
          rev
        )
    ) %>% 
  ddply(
    .(study, item),
    function(i){

      lm1 <- lm(ans ~ ideol * corr,
                data = i)
      
      lm1 %>% 
        emmeans(~corr | ideol, 
                at = list(
                  ideol = c(1, 4, 7)
                )
        ) %>% 
        pairs(reverse = T) %>% 
        tidy %>% 
        select(
          ideol:p.value
        ) %>% 
        rename(
          "ycat" = ideol
        ) %>% 
        rbind(
          lm1 %>%
            emmeans(~ideol,
                    at = list(
                      ideol = c(1,  7)
                    )
            ) %>%
            pairs %>%
            tidy %>%
            mutate(ycat = "ideol") %>% 
            select(ycat, estimate:p.value)
        )
    }
  ) %>% 
  tbl_df %>% 
  mutate(
    ycat = ycat %>% 
      mapvalues(
        c(1, 4, 7, "ideol"),
        c("Correction among liberals",
          "Correction among moderates",
          "Correction among conservatives",
          "Absolute value of ideological effect")
        ) %>%
      factor(
        c("Correction among liberals",
          "Correction among moderates",
          "Correction among conservatives",
          "Absolute value of ideological effect")
      ),
    estimate = ycat %>%
      str_detect("Absolute") %>% 
      ifelse(
        estimate %>% 
          abs,
        estimate
      ),
    lab = estimate %>%
      round(1) %>% 
      str_replace("0.", ".") %>% 
      equals("0") %>% 
      ifelse(
        estimate %>%
          round(2) %>% 
          str_replace("0.", "."),
        estimate %>%
          round(1) %>% 
          str_replace("0.", ".")
      ),
    xmin = estimate %>% 
      subtract(
        std.error %>% 
          multiply_by(
            1.96
          )
      ),
    xmax = estimate %>%
      add(
        std.error %>%
          multiply_by(
            1.96
          )
      ),
    study = study %>%  
      mapvalues(
        1:6,
        c(1:4,
          c("5: Turk",
            "5: Lucid") %>% 
            rev
        )
      ) %>% 
      factor(
        c(1:4,
          c("5: Turk",
            "5: Lucid")
        )
      ),
    sig = ifelse(
      (
        t1$estimate %>%
          is_less_than(0) &
          t1$xmax %>%
          is_less_than(0)
      )  |
        (
          t1$estimate %>%
            is_greater_than(0) &
            t1$xmin %>%
            is_greater_than(0)
        ),
      "significant",
      "insignificant"
      )
    ) %>% 
    {
  
      inp <- .
      
      
      inp$item %<>% 
        factor(
          inp %>%
            filter(
              ycat %>%
                str_detect(
                  "Absolute"
                )
            ) %>%
            arrange(estimate) %>% 
            use_series(
              item
            ) 
        )
      
      inp
      
      }  %>% 
  ggplot() +
  geom_vline(
    aes(xintercept = 0),
    linetype = 3,
    data = t1) +
  geom_linerangeh(
    aes(y = item, xmin = xmin, xmax = xmax),
    shape = 21,
    size = 1/2,
    color = "grey55"
  ) +
  geom_point(
    aes(estimate, item, fill = sig),
    shape = 21,
    size = 2,
    color = "grey40"
  ) +
  scale_fill_manual(
    values = c("grey98", "grey40"),
    labels = c("p >= .05", "p < .05") 
  ) +
  facet_grid(
    study ~ ycat, 
    scales = "free", 
    space = "free",
    labeller = label_wrap_gen(
      width = 25
    )
  ) +
  scale_y_discrete(
    breaks = t1$item %>% 
      levels,
    labels = t1$item %>% 
      levels %>% 
      str_sub(, -4)
  ) +
  scale_x_continuous(
    breaks = -2:2
  ) +
  labs(
    x = "Difference along 5 point scale",
    y = "",
    fill = "Significance"
  )

