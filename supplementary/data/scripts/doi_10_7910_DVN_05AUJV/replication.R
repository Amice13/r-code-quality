library(tidyverse)
library(marginaleffects)
library(patchwork)

## data loading---
UK <- read_csv("data/UKdata.csv")
JP <- read_csv("data/JPdata.csv")

UK <- UK %>% 
  mutate(covid_treatment = factor(covid_treatment,
                                  levels = c("Control",
                                             "Algorithm",
                                             "Mobile phone",
                                             "Interview"))) %>% 
  mutate(sex = factor(sex, levels = c("male", "female"))) %>% 
  mutate(univ = factor(univ))

JP <- JP %>% 
  mutate(covid_treatment = factor(covid_treatment,
                                  levels = c("Control",
                                             "Algorithm",
                                             "Mobile phone",
                                             "Interview"))) %>% 
  mutate(sex = factor(sex, levels = c("male", "female"))) %>% 
  mutate(univ = factor(univ))


## Figure 1----
anova_UK1 <- UK %>% 
  filter(!is.na(covid_1)) %>% 
  anova_test(covid_1 ~ covid_treatment)

pwc_UK1 <- UK %>% 
  filter(!is.na(covid_1)) %>% 
  tukey_hsd(covid_1 ~ covid_treatment)

UK_f1 <-UK %>% 
  ggbarplot(x = "covid_treatment", y = "covid_1", add = "mean_se",
            label = TRUE, lab.vjust = 2, lab.nb.digits = 2) +
  stat_pvalue_manual(pwc_UK1[pwc_UK1$term == "covid_treatment", ], label = "p.adj", y.position = c(5.5,6,6.5,7,7.5, 8)) +
  labs(x = "", y = "", title = "United Kingdom",
       subtitle = get_test_label(anova_UK1[anova_UK1$Effect == "covid_treatment", ], detailed = TRUE),
       caption = get_pwc_label(pwc_UK1[pwc_UK1$term == "covid_treatment",]))
UK_f1

anova_JP1 <- JP %>% 
  filter(!is.na(covid_1)) %>% 
  anova_test(covid_1 ~ covid_treatment)
pwc_JP1 <- JP %>% 
  filter(!is.na(covid_1)) %>% 
  tukey_hsd(covid_1 ~ covid_treatment)

JP_f1 <- JP %>% 
  ggbarplot(x = "covid_treatment", y = "covid_1", add = "mean_se",
            label = TRUE, lab.vjust = 2, lab.nb.digits = 2) +
  stat_pvalue_manual(pwc_JP1[pwc_JP1$term == "covid_treatment", ], label = "p.adj", y.position = c(5,5.5,6,6.5,7,7.5)) +
  labs(x = "", y = "", title = "Japan",
       subtitle = get_test_label(anova_JP1[anova_JP1$Effect == "covid_treatment", ], detailed = TRUE),
       caption = get_pwc_label(pwc_JP1[pwc_JP1$term == "covid_treatment",]))
JP_f1

Fig1 <- UK_f1 + JP_f1
Fig1
ggsave("Fig1.jpeg", plot = Fig1, dpi = 400, width = 8.2, height = 5)


## Figure 2----
anova_JP2 <- JP %>% 
  filter(!is.na(covid_3)) %>% 
  anova_test(covid_3 ~ covid_treatment)

pwc_JP2 <- JP %>% 
  filter(!is.na(covid_3)) %>% 
  tukey_hsd(covid_3 ~ covid_treatment)

JP_f2 <- JP %>% 
  ggbarplot(x = "covid_treatment", y = "covid_3", add = "mean_se",
            label = TRUE, lab.vjust = 2, lab.nb.digits = 2) +
  stat_pvalue_manual(pwc_JP2[pwc_JP2$term == "covid_treatment", ], label = "p.adj", y.position = c(5,5.5,6,6.5,7,7.5)) +
  labs(x = "", y = "", title = "Japan",
       subtitle = get_test_label(anova_JP2[anova_JP2$Effect == "covid_treatment", ], detailed = TRUE),
       caption = get_pwc_label(pwc_JP2[pwc_JP2$term == "covid_treatment",]))
JP_f2


anova_UK2 <- UK %>% 
  filter(!is.na(covid_3)) %>% 
  anova_test(covid_3 ~ covid_treatment)
pwc_UK2 <- UK %>% 
  filter(!is.na(covid_3)) %>% 
  tukey_hsd(covid_3 ~ covid_treatment)

UK_f2 <- UK %>% 
  ggbarplot(x = "covid_treatment", y = "covid_3", add = "mean_se",
            label = TRUE, lab.vjust = 2, lab.nb.digits = 2) +
  stat_pvalue_manual(pwc_UK2[pwc_UK2$term == "covid_treatment", ], label = "p.adj", y.position = c(5,5.5,6,6.5,7,7.5)) +
  labs(x = "", y = "", title = "United Kingdom",
       subtitle = get_test_label(anova_UK2[anova_UK2$Effect == "covid_treatment", ], detailed = TRUE),
       caption = get_pwc_label(pwc_UK2[pwc_UK2$term == "covid_treatment",]))
UK_f2
Fig2 <- UK_f2 + JP_f2
Fig2
ggsave("Fig2.jpeg", plot = Fig2, dpi = 400, width = 8.2, height = 5)


## Figure 3----
model1 <- lm(covid_1 ~ covid_treatment*gov_t + sex + age + univ, data = UK )
model2 <- lm(covid_1 ~ covid_treatment*gov_t + sex + age + univ, data = JP )


int_1 <- slopes(mod = model1, 
                variables = "covid_treatment", 
                condition = "gov_t")

int_1 <- int_1 %>%
  mutate(
    contrast = recode(contrast,
                      "Algorithm - Control" = "Algorithm",
                      "Mobile phone - Control" = "Mobile phone",
                      "Interview - Control" = "Interview"),
    contrast = factor(contrast, levels = c("Algorithm", "Mobile phone", "Interview"))
  )

int_1 <- ggplot(int_1, aes(x = gov_t, y = estimate)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, colour = NA) +
  facet_wrap(~contrast) +
  labs(x = "", y = "", title = "United Kingdom") +
  ylim(-1, 0.8)
int_1


int_2 <- slopes(mod = model2, 
                variables = "covid_treatment", 
                condition = "gov_t2")

int_2 <- int_2 %>%
  mutate(
    contrast = recode(contrast,
                      "Algorithm - Control" = "Algorithm",
                      "Mobile phone - Control" = "Mobile phone",
                      "Interview - Control" = "Interview"),
    contrast = factor(contrast, levels = c("Algorithm", "Mobile phone", "Interview"))
  )

int_2 <- ggplot(int_2, aes(x = gov_t, y = estimate)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, colour = NA) +
  facet_wrap(~contrast) +
  labs(x = "", y = "", title = "Japan") +
  ylim(-1, 0.8)
int_2

Fig3 <- int_1 + int_2
Fig3
ggsave("Fig3.jpeg", plot = Fig3, dpi = 400, width = 8.2, height = 4)
