## Adaptive Experimental Design: Prospects and Applications in Political Science
## Conducts simulations of individual experiments
## Produces Figure 1 & Figure 2, and csvs with point values for these figures

version

# install.packages(c('ggrepel', 'cowplot', 'grid', 'gridExtra', 'ggpubr'))

library(ggrepel)
library(cowplot)
library(grid)
library(gridExtra)
library(ggpubr)
set.seed(95126)


# Set options ----
source('utils.R')
n <- 100
periods <- 10
arms <- 9

# Case 1: clear winner ----
best_prob <- .2
other_prob <- .1 
probs <- c(best_prob, rep(other_prob, arms-1))

arm_names <- c('best', paste0('alt', 1:(arms-1)))

adapt <- sim_out(probs = probs)
adapt_te <- sim_out_te(probs = probs)
stat <- sim_out(probs = probs, static = TRUE)

# *overtime
# adaptive and static
gg_df_adapt <- as.data.frame(matrix(rbind(rep(1/arms, arms), adapt$ppmat), 
                                    nrow = periods+1) )
gg_df_stat <- as.data.frame(matrix(rbind(rep(1/arms, arms), stat$ppmat), 
                                   nrow = periods+1) )

names(gg_df_adapt) <- names(gg_df_stat) <- arm_names

gg_df_adapt$batch <- gg_df_stat$batch  <- 0:periods

gg_df <- bind_rows(
  list(
    static = melt(gg_df_stat, id.vars = 'batch'),
    TS = melt(gg_df_adapt, id.vars = 'batch')),
  .id = 'type'
) %>% 
  mutate(p.cat = as.factor((variable != 'best')*2))


gg_df1 <- gg_df

# control-augmented
gg_df_adapt <- as.data.frame(matrix(rbind(rep(1/arms, arms), adapt_te$ppmat),
                                    nrow = periods+1) )
gg_df_adaptn <- as.data.frame(matrix(rbind(rep(1/arms, arms), adapt_te$nmat),
                                     nrow = periods+1) )

names(gg_df_adapt) <- names(gg_df_adaptn) <- arm_names

gg_df_adapt$batch <- gg_df_adaptn$batch  <- 0:periods

gg_df <- bind_rows(
  list(
    `sample size, cumulative` = melt(gg_df_adaptn, id.vars = 'batch'),
    `posterior probability` = melt(gg_df_adapt, id.vars = 'batch')),
  .id = 'type'
) %>%
  mutate(p.cat = as.factor(case_when(variable == 'best' ~ 0,
                                     variable == 'alt2' ~ 3,
                                     TRUE ~ 2)))

gg_df1_te <- gg_df


# Case 2: no clear winner ----

best_prob <- .11
other_prob <- .1
probs <- c(best_prob, rep(other_prob, arms-1))

adapt <- sim_out(probs = probs)
stat <- sim_out(probs = probs, static = TRUE)
adapt_te <- sim_out_te(probs = probs)

# *overtime
# adaptive and static
gg_df_adapt <- as.data.frame(matrix(rbind(rep(1/arms, arms), adapt$ppmat), 
                                    nrow = periods+1) )
gg_df_stat <- as.data.frame(matrix(rbind(rep(1/arms, arms), stat$ppmat), 
                                   nrow = periods+1) )

names(gg_df_adapt) <- names(gg_df_stat) <- arm_names

gg_df_adapt$batch <- gg_df_stat$batch  <- 0:periods

gg_df <- bind_rows(
  list(
    static = melt(gg_df_stat, id.vars = 'batch'),
    TS = melt(gg_df_adapt, id.vars = 'batch')),
  .id = 'type'
) %>% 
  mutate(p.cat = as.factor((variable != 'best')*2))


gg_df2 <- gg_df


# control-augmented
gg_df_adapt <- as.data.frame(matrix(rbind(rep(1/arms, arms), adapt_te$ppmat),
                                    nrow = periods+1) )
gg_df_adaptn <- as.data.frame(matrix(rbind(rep(1/arms, arms), adapt_te$nmat),
                                     nrow = periods+1) )

names(gg_df_adapt) <- names(gg_df_adaptn) <- arm_names

gg_df_adapt$batch <- gg_df_adaptn$batch  <- 0:periods

gg_df <- bind_rows(
  list(
    `sample size, cumulative` = melt(gg_df_adaptn, id.vars = 'batch'),
    `posterior probability` = melt(gg_df_adapt, id.vars = 'batch')),
  .id = 'type'
) %>%
  mutate(p.cat = as.factor(case_when(variable == 'best' ~ 0,
                                     variable == 'alt2' ~ 3,
                                     TRUE ~ 2)))

gg_df2_te <- gg_df

# Case 3: competing second best ----

best_prob <- .2
second_best <- .18
other_prob <- .1
probs <- c(best_prob, second_best, rep(other_prob, arms-2))

adapt <- sim_out(probs = probs)
stat <- sim_out(probs = probs, static = TRUE)
adapt_te <- sim_out_te(probs = probs)

# *overtime
# adaptive and static
gg_df_adapt <- as.data.frame(matrix(rbind(rep(1/arms, arms), adapt$ppmat), 
                                    nrow = periods+1) )
gg_df_stat <- as.data.frame(matrix(rbind(rep(1/arms, arms), stat$ppmat), 
                                   nrow = periods+1) )


names(gg_df_adapt) <- names(gg_df_stat) <- arm_names

gg_df_adapt$batch <- gg_df_stat$batch  <- 0:periods

gg_df <- bind_rows(
  list(
    static = melt(gg_df_stat, id.vars = 'batch'),
    TS = melt(gg_df_adapt, id.vars = 'batch')),
  .id = 'type'
) %>% 
  mutate(p.cat = as.factor(case_when(variable == 'best' ~ 0,
                                     variable == 'alt1' ~ 1,
                                     TRUE ~ 2)))

gg_df3 <- gg_df

# control-augmented
gg_df_adapt <- as.data.frame(matrix(rbind(rep(1/arms, arms), adapt_te$ppmat),
                                    nrow = periods+1) )
gg_df_adaptn <- as.data.frame(matrix(rbind(rep(1/arms, arms), adapt_te$nmat),
                                     nrow = periods+1) )

names(gg_df_adapt) <- names(gg_df_adaptn) <- arm_names

gg_df_adapt$batch <- gg_df_adaptn$batch  <- 0:periods

gg_df <- bind_rows(
  list(
    `sample size, cumulative` = melt(gg_df_adaptn, id.vars = 'batch'),
    `posterior probability` = melt(gg_df_adapt, id.vars = 'batch')),
  .id = 'type'
) %>%
  mutate(p.cat = as.factor(case_when(variable == 'best' ~ 0,
                                     variable == 'alt2' ~ 3,
                                     variable == 'alt1' ~ 1,
                                     TRUE ~ 2)))
gg_df3_te <- gg_df


# Faceted across cases ----
# * Adaptive and static ----
# ** [Figure 1] ----
gg_df <- bind_rows(
  'Case 1: Clear winner' = gg_df1,
  'Case 2: No clear winner' = gg_df2,
  'Case 3: Competing second best' = gg_df3,
  .id = 'case'
) %>% 
  mutate(p.cat = factor(p.cat),
         type = factor(type, levels = c('TS', 'static')))

text_df <- group_by(gg_df, variable, type, case) %>%
  filter(batch == max(batch)) %>% 
  ungroup() %>% 
  mutate(profile = case_when(variable == 'best' ~ 'True best arm',
                             variable == 'alt1' & 
                               case == 'Case 3: Competing second best' ~
                               'Second best arm'))

g <- ggplot(gg_df,
            aes(
              x = batch,
              y = value,
              group = variable,
              color = p.cat,
              linetype = p.cat
            )) +
  geom_line() +
  theme_bw() +
  facet_wrap( ~ case + type,
              nrow = 3,
              labeller = label_wrap_gen(width = 40, multi_line = FALSE)) +
  coord_cartesian(xlim = c(0, 14.5),
                  ylim = c(0, 1),
                  clip = 'off') +
  scale_x_continuous(breaks = c(seq(1, 10, 1))) +
  scale_y_continuous(breaks = c(seq(0, 1, 0.25))) +
  scale_colour_manual(
    name = 'Position in last period',
    breaks = levels(gg_df$p.cat),
    labels = levels(gg_df$p.cat),
    values = gray(seq(.1,.6, len = length(levels(gg_df$p.cat)) ))) +
  theme(
    legend.position = 'none',
    strip.background = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ylab('Posterior probability of being the best arm') +
  xlab('Batch number') +
  geom_text_repel(
    data = text_df,
    aes(label = profile),
    nudge_x = 5,
    hjust = 1,
    segment.size = .2,
    seed = 343,
    direction = 'y',
    size = 3
  )

g

# ggsave(
#   filename = 'figure1.pdf',
#   plot = g,
#   width = 6.5,
#   height = 6
# )

# ggsave(
#   filename = 'figure1_color.pdf',
#   plot = g + scale_colour_discrete(),
#   width = 6.5,
#   height = 6
# )

gg_df <- gg_df %>% 
  filter(batch == periods) %>% 
  select(case, type, batch, variable, value)

head(gg_df)

# write_csv(gg_df, 'figure1_values.csv')


# * Control-augmented ----
# ** [Figure 2] ----
gg_df <- bind_rows(
  'Case 1: Clear winner, TS, control-augmented' = gg_df1_te,
  'Case 2: No clear winner, TS, control-augmented' = gg_df2_te,
  'Case 3: Competing second best, TS, control-augmented' = gg_df3_te,
  .id = 'case'
) %>%
  mutate(p.cat = factor(p.cat))

text_df <- group_by(gg_df, variable, type, case) %>%
  filter(batch == max(batch)) %>%
  ungroup() %>%
  mutate(profile = 
           case_when(variable == 'best' ~ 'True best arm',
                     variable == 'alt2' ~ 'Control arm',
                     variable == 
                       'alt1' & case == 
                       'Case 3: Competing second best, TS, control-augmented' ~
                       'Second best arm'))

g1 <- ggplot(filter(gg_df, type == 'posterior probability'), 
             aes(x=batch,
                 y=value,
                 group=variable,
                 color=p.cat,
                 linetype=p.cat)) +
  geom_line() +
  theme_bw() +
  facet_wrap(~ case, nrow = 3, 
             labeller = label_wrap_gen(width = 80, multi_line=TRUE),
             scales = 'free_y') +
  coord_cartesian(xlim = c(0, 14.5),
                  ylim = c(0, 1),
                  clip = 'off') +
  scale_x_continuous(breaks = c(seq(1, 10, 1) )) +
  scale_colour_manual(
    name = 'Position in last period',
    breaks = levels(gg_df$p.cat),
    labels = levels(gg_df$p.cat),
    values = gray(seq(.1,.6, len = length(levels(gg_df$p.cat)) ))) +
  theme(legend.position = 'none', strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        strip.text = element_text(colour = 'white')) +
  ylab('Posterior probability of being the best arm') +
  geom_text_repel(
    data = filter(text_df, type == 'posterior probability'),
    aes(label = profile),
    nudge_x = 5,
    hjust = 1,
    segment.size = .2,
    seed = 343,
    direction = 'y',
    size = 3
  ) 


g2 <- ggplot(filter(gg_df, type == 'sample size, cumulative'), 
             aes(x=batch,
                 y=value,
                 group=variable,
                 color=p.cat,
                 linetype=p.cat)) +
  geom_line() +
  theme_bw() +
  facet_wrap(~ case, nrow = 3, 
             labeller = label_wrap_gen(width = 80, multi_line=TRUE),
             scales = 'free_y') +
  coord_cartesian(xlim = c(0, 14.5),
                  ylim = c(0, 350),
                  clip = 'off') +
  scale_x_continuous(breaks = c(seq(1, 10, 1) )) +
  scale_colour_manual(
    name = 'Position in last period',
    breaks = levels(gg_df$p.cat),
    labels = levels(gg_df$p.cat),
    values = gray(seq(.1,.6, len = length(levels(gg_df$p.cat)) ))) +
  theme(legend.position = 'none', strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        strip.text = element_text(colour = 'white')
  ) +
  ylab('Cumulative sample') +
  geom_text_repel(
    data = filter(text_df, type == 'sample size, cumulative'),
    aes(label = profile),
    nudge_x = 5,
    hjust = 1,
    segment.size = .2,
    seed = 343,
    direction = 'y',
    size = 3
  ) 

g <- annotate_figure(ggarrange(g1, g2), bottom = ' ') + 
  annotate("text", x = .5, y = .97, 
           label = 'Case 1: Clear winner, TS, control augmented') + 
  annotate("text", x = .5, y = .66, 
           label = 'Case 2: No clear winner, TS, control augmented') + 
  annotate("text", x = .5, y = .355, 
           label = 'Case 3: Competing second best, TS, control augmented') +
  annotate("text", x = .5, y = .03, 
           label = 'Batch number')

g

# ggsave(filename = 'figure2.pdf',
#        plot = g,
#        width=6.5,
#        height=6)

gc <- annotate_figure(ggarrange(g1 + scale_colour_discrete(), 
                                g2 + scale_colour_discrete()), 
                      bottom = ' ') + 
  annotate("text", x = .5, y = .97, 
           label = 'Case 1: Clear winner, TS, control augmented') + 
  annotate("text", x = .5, y = .66, 
           label = 'Case 2: No clear winner, TS, control augmented') + 
  annotate("text", x = .5, y = .355, 
           label = 'Case 3: Competing second best, TS, control augmented') +
  annotate("text", x = .5, y = .03, 
           label = 'Batch number')

# ggsave(filename = 'figure2_color.pdf',
#        plot = gc,
#        width=6.5,
#        height=6)

gg_df <- gg_df %>%
  filter(batch == periods) %>%
  select(case, type, batch, variable, value)

head(gg_df)

# write_csv(gg_df, 'figure2_values.csv')
