## Adaptive Experimental Design: Prospects and Applications in Political Science
## Code for Study 2: misperceptions, analysis
## Produces Figures 5, 6, and Appendix Figures F13 and F14; additionally csv
## reporting response frequency counts under the control condition, as 
## referenced in the text. 

version

# install.packages(c('ggrepel', 'coefplot'))

library(ggrepel)
library(coefplot)
set.seed(95126)
iter <- 1e3 # iterations

# Set options ----
source('utils.R')

mturk_bandit <- read_rds('study_2_clean.rds')
pmat <- read_rds('study_2_probabilities.rds')

# ** [study_2_response_control.csv] ----
# Reports response to the outcome questions for those who responded to questions
# in the control condition only, as reported in the text. 

study_2_response_control <- mturk_bandit %>% 
  filter(Z == 'control', Y_no_response_sum == 0) %>% 
  group_by(pid) %>% 
  summarize(deficit = sum(Y_deficit)/sum(Y_deficit_no_response==0),
            nfi = sum(Y_nfi)/sum(Y_nfi_no_response==0),
            unemp = sum(Y_unemp)/sum(Y_unemp_no_response==0))

head(study_2_response_control)

# write_csv(study_2_response_control, 'study_2_response_control.csv')


# Group means ----
# Reshape the data for analysis
mturk_long <- mturk_bandit %>% 
  gather(key = 'out', value = 'Y', c('Y_deficit', 'Y_unemp', 'Y_nfi'))%>% 
  mutate(Y_complete = case_when(
    (out == 'Y_deficit' & Y_deficit_no_response == 1) ~ NA_real_,
    (out == 'Y_unemp' & Y_unemp_no_response == 1) ~ NA_real_,
    (out == 'Y_nfi' & Y_nfi_no_response == 1) ~ NA_real_,
    TRUE ~ Y))

levels_l <- paste0(toupper(substr(levels(mturk_bandit$Z), 1, 1)), 
                   substr(levels(mturk_bandit$Z), 2, 1e3))
levels_l[which(levels_l == 'Extratime')] <- 'Extra Time'

# * Batch 10 estimates ----
# ** [Figure 6] ----

fit_d <- lm_robust(Y ~ Z, 
                   data = filter(mturk_long, pid == 'democrat'), 
                   cluster = id, weights = weights)
fit_r <- lm_robust(Y ~ Z, 
                   data = filter(mturk_long, pid == 'republican'), 
                   cluster = id, weights = weights)

fit_dcv <- lm_robust(Y ~ Z
                     + follow_pol_pre + race_pre + educ_5_pre + 
                       female_pre + age_pre + pid_7_pre + ideo_7_pre, 
                     data = filter(mturk_long, pid == 'democrat'), 
                     cluster = id, weights = weights)
fit_rcv <- lm_robust(Y ~ Z
                     + follow_pol_pre + race_pre + educ_5_pre + 
                       female_pre + age_pre + pid_7_pre + ideo_7_pre, 
                     data = filter(mturk_long, pid == 'republican'), 
                     cluster = id, weights = weights)

lm_IPW <- bind_rows(
  `Democrats` = tidy(fit_d),
  `Republicans` = tidy(fit_r),
  .id = 'type'
)

lm_IPWcv <- bind_rows(
  `Democrats` = tidy(fit_dcv),
  `Republicans` = tidy(fit_rcv),
  .id = 'type'
)

gg_df <-
  bind_rows(
    `IPW-Adjusted` = lm_IPWcv,
    `IPW` = lm_IPW,
    .id = 'Estimates'
  ) %>% 
  filter(grepl('^Z', term)) %>% 
  mutate(
    term = gsub(
      pattern = 'Z',
      replacement = '',
      x = term),
    term = paste0(toupper(substr(term, 1, 1)), substr(term, 2, 1e3)),
    term = case_when(term == 'Extratime' ~ 'Extra Time',
                     TRUE ~ term),
    label = make_entry(estimate, std.error, p.value)
  )

label_df <- 
  gg_df %>%
  filter(type == 'Democrats', term == 'Lottery') %>%
  mutate(x = conf.low - 0.03)


g <-
  ggplot(gg_df, aes(estimate, term, color = Estimates, shape = Estimates)) +
  geom_point(position = position_dodgev(height = -0.4)) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0,
    position = position_dodgev(height = -0.4)
  ) +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_text(
    data = label_df,
    size = 2.5,
    aes(label = Estimates, x = x),
    position = position_dodgev(height = -0.4),
    hjust = 1
  ) +
  facet_wrap( ~ type) +
  theme_bw() +
  coord_cartesian(xlim = c(-.15, .15)) +
  geom_text(
    aes(label = label),
    position = position_dodgev(height = -1),
    size = 2.5,
    show.legend = FALSE
  ) +
  theme(
    strip.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = 'none'
  ) +
  scale_colour_manual(values = gray(seq(.1, .6, len = length(
    unique(gg_df$Estimates)
  ))))

g

# ggsave(
#   filename = 'figure6.pdf',
#   plot = g,
#   width = 6.5,
#   height = 4
# )

# ggsave(
#   filename = 'figure6_color.pdf',
#   plot = g + scale_colour_discrete(),
#   width = 6.5,
#   height = 4
# )



# Posterior probabilities ----
# ** [Figure 5] ----
gg_df <- bind_rows(
  list(
    'Democrats' = melt(pmat[['ppmat_dem']]),
    'Republicans' = melt(pmat[['ppmat_rep']])
  ),
  .id = 'type') %>% 
  group_by(type, variable) %>% 
  mutate(batch = 0:(n()-1) ) %>% 
  group_by(batch, type) %>% 
  mutate(p.cat = rank(-value)) %>%
  ungroup() %>% 
  mutate(Z = rep(rep(levels_l, each = n_distinct(batch)), times = 2)) %>% 
  group_by(Z, type) %>% 
  mutate(p.cat = p.cat[batch == 10]) %>% 
  ungroup() %>% 
  mutate(p.cat = factor(p.cat))


text_df <- filter(gg_df, batch == max(batch))

g <-
  ggplot( gg_df,
          aes(
            x = batch,
            y = value,
            color = p.cat
          )) +
  geom_line() +
  theme_bw() +
  facet_grid( ~ type) +
  coord_cartesian(xlim = c(0, 15),
                  ylim = c(0, max(gg_df$value) + .1),
                  clip = 'off') +
  scale_x_continuous(breaks = c(seq(1, 10, 1) )) +
  scale_y_continuous(breaks = c(seq(0, 1, 0.25) )) +
  scale_colour_manual(
    values = gray(seq(.1,.6, len = length(unique(gg_df$p.cat)) ))) + 
  theme(legend.position = 'none', strip.background = element_blank(),
        panel.grid.minor = element_blank()) +
  ylab('Posterior probability of being the best arm') +
  xlab('Batch number') +
  geom_text_repel(
    data = text_df,
    aes(label = Z),
    nudge_x = 5,
    nudge_y = .05,
    hjust = 1,
    segment.size = .2,
    seed = 343,
    direction = 'y',
    size = 3
  )

g

# ggsave(
#   filename = 'figure5.pdf',
#   plot = g,
#   width = 6.5,
#   height = 4
# )

# ggsave(
#   filename = 'figure5_color.pdf',
#   plot = g + scale_colour_discrete(),
#   width = 6.5,
#   height = 4
# )



# Analysis for Independents ----
# * Group means ----
# ** [Figure F13] ----
fit_i <- lm_robust(Y ~ Z, 
                   data = filter(mturk_long, pid == 'independent'), 
                   cluster = id, weights = weights)

fit_icv <- lm_robust(Y ~ Z
                     + follow_pol_pre + race_pre + educ_5_pre + 
                       female_pre + age_pre + pid_7_pre + ideo_7_pre, 
                     data = filter(mturk_long, pid == 'independent'), 
                     cluster = id, weights = weights)

gg_df <-
  bind_rows(
    `IPW-Adjusted` = tidy(fit_icv),
    `IPW` = tidy(fit_i),
    .id = 'Estimates'
  ) %>% 
  filter(grepl('^Z', term)) %>% 
  mutate(
    term = gsub(
      pattern = 'Z',
      replacement = '',
      x = term),
    term = paste0(toupper(substr(term, 1, 1)), substr(term, 2, 1e3))
  )
gg_df$term[gg_df$term=='Extratime'] <- 'Extra Time'

label_df <- 
  gg_df %>%
  filter(term == 'Lottery') %>%
  mutate(x = conf.low - 0.03)

g <- 
  ggplot(gg_df, aes(estimate, term, color = Estimates)) +
  geom_point(position = position_dodgev(height = -0.4)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 height = 0, 
                 position = position_dodgev(height = -0.4)) +
  geom_vline(xintercept = 0, 
             linetype = 'dashed') +
  geom_text(
    data = label_df,
    size = 2.5,
    aes(label = Estimates, x = x),
    position = position_dodgev(height = -0.4),
    hjust = 1
  ) +
  theme_bw() +
  coord_cartesian(xlim = c(-.4, .4)) +
  geom_text(
    aes(label = make_entry(
      gg_df$estimate, 
      gg_df$std.error, 
      gg_df$p.value
    )),
    position = position_dodgev(height = -1),
    size = 2.5,
    show.legend = FALSE
  ) +
  theme(
    strip.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = 'none'
  ) +
  scale_colour_manual(values = gray(seq(.1, .6, len = length(
    unique(gg_df$Estimates)
  ))))

g

# ggsave(
#   filename = 'figureF13.pdf',
#   plot = g,
#   width = 4,
#   height = 4
# )

# ggsave(
#   filename = 'figureF13_color.pdf',
#   plot = g + scale_colour_discrete(),
#   width = 4,
#   height = 4
# )


# Counterfactuals ----
# ** [Figure F14] ----
Zn <- length(unique(mturk_bandit$Z))
Zs <- unique(mturk_bandit$Z)

# * Static counterfactuals ----
# batch sizes for each partisan group if we had used static assignment

fit_d2l_static <- fit_r2l_static <- list()

for(j in 1:iter){
  # cat(c('\n Iter: ', j, ''))
  
  # assign distribution of treatment arms; varies slightly across experiments
  arm_n <- mturk_bandit %>% 
    group_by(pid) %>% 
    summarise(n = n(), .groups = 'keep') %>% 
    pull() %>% 
    sapply(., function(x) table(complete_ra(x, num_arms = Zn))) %>% 
    suppressWarnings()
  
  colnames(arm_n) <- sort(unique(mturk_bandit$pid))
  rownames(arm_n) <- Zs
  
  sample_l <- list()
  
  for(p in c('republican', 'democrat')){
    for(z in Zs){
      
      # resample based on pid, treatment condition
      new_mat <- mturk_bandit %>% 
        filter(
          pid == p,
          Z == z
        ) %>% 
        sample_n(arm_n[z,p], weight = weights, replace = TRUE)
      
      sample_l[[paste(p, z, sep = '_')]] <- new_mat
    }
  }
  sample_df <- do.call('rbind', sample_l) %>% 
    mutate(id = 1:n())
  
  sample_df_long <- sample_df %>% 
    gather(key = 'out', value = 'Y', c('Y_deficit', 'Y_unemp', 'Y_nfi')) %>% 
    mutate(Z = relevel(Z, 'control'))
  
  fit_d2l_static[[j]] <- tidy(lm_robust(Y ~ Z, 
                                        data = filter(sample_df_long, 
                                                      pid == 'democrat'), 
                                        cluster = id))
  
  fit_r2l_static[[j]] <- tidy(lm_robust(Y ~ Z, 
                                        data = filter(sample_df_long, 
                                                      pid == 'republican'), 
                                        cluster = id))
}

# Summarize bootstrap data

fit_d2_static <- do.call('rbind', fit_d2l_static) %>% 
  group_by(term) %>% 
  summarize(
    std.mean = mean(std.error),
    std.error = sd(estimate),
    estimate = mean(estimate),
    statistic = estimate/std.error,
    p.value = 2*pt(-abs(statistic), n()),
    conf.low = estimate - qt(.975, n())*std.error,
    conf.high = estimate + qt(.975, n())*std.error
  )

fit_r2_static <- do.call('rbind', fit_r2l_static) %>% 
  group_by(term) %>% 
  summarize(
    std.mean = mean(std.error),
    std.error = sd(estimate),
    estimate = mean(estimate),
    statistic = estimate/std.error,
    p.value = 2*pt(-abs(statistic), n()),
    conf.low = estimate - qt(.975, n())*std.error,
    conf.high = estimate + qt(.975, n())*std.error
  )

lm_static <- bind_rows(
  `Democrats` = fit_d2_static,
  `Republicans` = fit_r2_static,
  .id = 'type'
)



# Save initial IPW estimate vs static comparison
gg_df <-
  bind_rows(
    `Static` = lm_static,
    `IPW` = lm_IPW,
    .id = 'Estimates'
  ) %>% 
  filter(grepl('^Z', term)) %>% 
  mutate(
    term = gsub(
      pattern = 'Z',
      replacement = '',
      x = term),
    term = paste0(toupper(substr(term, 1, 1)), substr(term, 2, 1e3)),
    term = case_when(term == 'Extratime' ~ 'Extra Time',
                     TRUE ~ term)
  )

label_df <- 
  gg_df %>%
  filter(type == 'Democrats', term == 'Lottery') %>%
  mutate(x = conf.low - 0.03)

g <- 
  ggplot(gg_df, aes(estimate, term, color = Estimates)) +
  geom_point(position = position_dodgev(height = -0.4)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 height = 0, 
                 position = position_dodgev(height = -0.4)) +
  geom_vline(xintercept = 0, 
             linetype = 'dashed') +
  geom_text(
    data = label_df,
    size = 2.5,
    aes(label = Estimates, x = x),
    position = position_dodgev(height = -0.4),
    hjust = 1
  ) +
  facet_wrap( ~ type) +
  theme_bw() +
  coord_cartesian(xlim = c(-.15, .15)) + 
  geom_text(aes(label = make_entry(estimate, 
                                   std.error, 
                                   p.value)),
            position = position_dodgev(height = -1), 
            size = 2.5, 
            show.legend = FALSE) +
  theme(strip.background = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = 'none') +
  scale_colour_manual(
    values = gray(seq(.1,.6, len = length(unique(gg_df$Estimates)) )))

g

# ggsave(
#   filename = 'figureF14.pdf',
#   plot = g,
#   width = 6.5,
#   height = 4
# )

# ggsave(
#   filename = 'figureF14_color.pdf',
#   plot = g + scale_colour_discrete(),
#   width = 6.5,
#   height = 4
# )
