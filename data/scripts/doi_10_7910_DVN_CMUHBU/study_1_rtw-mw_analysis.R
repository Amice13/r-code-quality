## Adaptive Experimental Design: Prospects and Applications in Political Science
## Code for Study 1: right-to-work and minimum wage, analysis
## Produces Figure 3 & Figure 4

version

# install.packages(c('ggrepel', 'coefplot', 'forcats'))

library(ggrepel)
library(coefplot)
library(forcats)

# Set options ----
source('utils.R')

mturk_bandit <- read_rds('study_1_clean.rds')


# Treatment frequency counts ----
# Referenced in text
(rtw_counts <- mturk_bandit %>% count(Z_rtw))
(mw_counts <- mturk_bandit %>% count(Z_mw))

# write_csv(rtw_counts, 'study_1_rtw_counts.csv')
# write_csv(mw_counts, 'study_1_mw_counts.csv')


# Group means ----
# ** [Figure 4] ----
fit_1 <-
  lm_robust(Y_mw ~ Z_mw - 1, weights = weights_mw, data = mturk_bandit)
fit_2 <-
  lm_robust(Y_rtw ~ Z_rtw - 1, weights = weights_rtw, data = mturk_bandit)

gg_df <- bind_rows(
  `Minimum wage` = tidy(fit_1),
  `Right-to-work` = tidy(fit_2),
  .id = 'topic'
) %>%
  mutate(
    label = gsub(
      pattern = 'Z_mw|Z_rtw',
      replacement = '',
      x = term
    ),
    label = fct_reorder(factor(label), estimate)
  )

g <-
  ggplot(gg_df, aes(
    x = estimate,
    y = label,
    xmin = conf.low,
    xmax = conf.high
  )) +
  geom_point() +
  geom_errorbarh(height = 0) +
  facet_wrap( ~ topic, scales = 'free') +
  theme_bw() + 
  geom_text(aes(label = paste0(format_num(gg_df$estimate, 3),
                               " ",
                               add_parens(gg_df$std.error, 3))),
            size = 2.5, 
            show.legend = FALSE,
            nudge_y = .3,
            nudge_x = -.05) +
  theme(strip.background = element_blank(),
        axis.title.y = element_blank()) +
  xlim(0, 1) + 
  xlab('Average proportion of respondents supporting the measure')

g

# ggsave(
#   filename = 'figure4.pdf',
#   plot = g,
#   width = 6.5,
#   height = 4
# )


# Posterior probabilities ----
# ** [Figure 3] ----
# Right to work

summary_df <-
  mturk_bandit %>%
  group_by(Z_rtw, batch) %>%
  summarize(successes = sum(Y_rtw),
            trials = length(Y_rtw)) %>%
  ungroup() %>% 
  complete(Z_rtw, batch, fill = list(successes = 0, trials = 0)) %>%
  group_by(Z_rtw) %>%
  # best_binomial_bandit operates on cumulative totals
  mutate(cumulative_successes = cumsum(successes),
         cumulative_trials = cumsum(trials)) %>%
  group_by(batch) %>%
  mutate(posterior_prob = 
           best_binomial_bandit(cumulative_successes, cumulative_trials))

zeros_df <-
  summary_df %>%
  group_by(Z_rtw) %>%
  summarize(batch = 0, posterior_prob = 1/8)

gg_df_rtw <- bind_rows(summary_df, zeros_df)

# minimum wage
summary_df <-
  mturk_bandit %>%
  group_by(Z_mw, batch) %>%
  summarize(successes = sum(Y_mw),
            trials = length(Y_mw)) %>%
  ungroup() %>% 
  complete(Z_mw, batch, fill = list(successes = 0, trials = 0)) %>%
  group_by(Z_mw) %>%
  # best_binomial_bandit operates on cumulative totals
  mutate(cumulative_successes = cumsum(successes),
         cumulative_trials = cumsum(trials)) %>%
  group_by(batch) %>%
  mutate(posterior_prob = best_binomial_bandit(cumulative_successes, 
                                               cumulative_trials))

zeros_df <-
  summary_df %>%
  group_by(Z_mw) %>%
  summarize(batch = 0, posterior_prob = 1/10)

gg_df_mw <- bind_rows(summary_df, zeros_df)


gg_df <-
  bind_rows(`Minimum wage` = gg_df_mw,
            `Right-to-work` = gg_df_rtw,
            .id = 'topic') %>% 
  mutate(Z = coalesce(as.character(Z_mw), as.character(Z_rtw))) %>% 
  group_by(batch, topic) %>%
  mutate(p.cat = rank(-posterior_prob)) %>%
  ungroup() %>%
  group_by(Z) %>% 
  mutate(p.cat = p.cat[batch == 10]) %>% 
  ungroup() %>% 
  mutate(p.cat = factor(p.cat))


text_df <- filter(gg_df, batch == 10)

g <-
  ggplot(gg_df,
         aes(
           x = batch,
           y = posterior_prob,
           group = Z,
           color = p.cat
         )) +
  geom_line() +
  ylim(0, 1) +
  facet_wrap(~ topic) +
  geom_text_repel(
    data = text_df,
    aes(label = Z),
    hjust = -1,
    vjust = .5,
    force = 10,
    seed = 343,
    direction = 'y', 
    segment.alpha = .25
  ) +
  theme_bw() +
  scale_colour_manual(
    name = 'Position in last period',
    values = gray(seq(.1,.6, len = length(unique(gg_df$p.cat))) )) +
  coord_cartesian(xlim = c(0, 20)) + 
  scale_x_continuous(breaks = 1:10) +
  theme(legend.position = 'none', strip.background = element_blank(),
        panel.grid.minor = element_blank()) +
  ylab('Posterior probability of being the best arm') +
  xlab('Batch number')

g

# ggsave(
#   filename = 'figure3.pdf',
#   plot = g,
#   width = 6.5,
#   height = 4
# )


# ggsave(
#   filename = 'figure3_color.pdf',
#   plot = g + scale_color_discrete(),
#   width = 6.5,
#   height = 4
# )
