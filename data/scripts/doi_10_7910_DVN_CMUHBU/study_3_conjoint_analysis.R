## Adaptive Experimental Design: Prospects and Applications in Political Science
## Code for Study 3: conjoint, analysis
## Produces Figure G15 & Figure G16

version

# install.packages(c('ggrepel', 'MCMCpack'))

library(ggrepel)
MCMCprobit <- MCMCpack::MCMCprobit
set.seed(95126)


# Set options ----
source('utils.R')

bandit_factor <- read_rds('study_3_clean.rds')
bandit_factor_stat <- read_rds('study_3_stat_clean.rds')

# Posterior probabilities ----
outcome_model <- formula(Y_binary ~ Z_1 + Z_2 + Z_3 + Z_4)


Z_combs <- data.frame(expand.grid(Z_1 = factor(unique(bandit_factor$Z_1),
                                               labels = c('No limits',
                                                          '$10,000',
                                                          '100,000',
                                                          '$1,000,000')), 
                                  Z_2 = factor(unique(bandit_factor$Z_2),
                                               labels = c('Allow all',
                                                          'Prohibit candidate;\n allow PAC',
                                                          'Prohibit all')), 
                                  Z_3 = factor(unique(bandit_factor$Z_3),
                                               labels = c('Prohibit all',
                                                          '1:1 match',
                                                          '5:1 match',
                                                          'Maintain status quo')), 
                                  Z_4 = factor(unique(bandit_factor$Z_4),
                                               labels = c('Disclose > $200',
                                                          'Disclose > $500',
                                                          'Eliminate',
                                                          'Disclose all'))),
                 Y_binary = 1)



# Identify profiles, to match to posterior probabilities
mm <- model.matrix(outcome_model, data = Z_combs)
mm_bandit <- model.matrix(outcome_model, data = bandit_factor)
mm_stat <- model.matrix(outcome_model, data = bandit_factor_stat)

#** Adaptive ----
pmat_bandit <- pmat_stat <- as_tibble(
  data.frame( 0:10, matrix(NA_real_, ncol = 192, nrow = 11)) ) 
colnames(pmat_bandit) <- colnames(pmat_stat) <- c('batch', 1:192)

# period 0, priors = posteriors
pmat_bandit[1, 2:193] <- pmat_stat[1, 2:193] <- 1/192

for(i in 1:10){
  # fit bayesian model with uninformative priors (all beta = 0)
  vals <- which(bandit_factor$batch %in% unique(bandit_factor$batch)[1:i])
  glm_MC <- MCMCprobit(outcome_model, mcmc = 1e5, 
                       data = bandit_factor[vals,])
  # predictions based on simulations
  post <- glm_MC %*% t(mm) 
  
  # calculate posterior probabilities of being the best
  probs <- prob_winner(post)
  pmat_bandit[(i+1), 2:193] <- as_tibble_row(matrix(probs, nrow = 1), 
                                             .name_repair = ~ as.character(c(1:192)))
  cat(c(i, '... '))
}

#** Static ----

for(i in 1:10){
  # fit bayesian model with uninformative priors (all beta = 0)
  vals <- which(bandit_factor_stat$batch %in% 
                  unique(bandit_factor$batch)[1:i])
  glm_MC <- MCMCprobit(outcome_model, mcmc = 1e5, 
                       data = bandit_factor_stat[vals,])
  # predictions based on simulations
  post <- glm_MC %*% t(mm)
  
  # calculate posterior probabilities of being the best
  probs <- prob_winner(post)
  pmat_stat[(i+1), 2:193] <- as_tibble_row(matrix(probs, nrow = 1), 
                                           .name_repair = ~ as.character(c(1:192)))
  cat(c(i, '... '))
}

# Over time ----
#* Joint faceted  ----
# ** [Figure G16] ----
breaks <- c(0, 10, 50, 100, 192) # breaks for color or line coding

gg_df <- melt(pmat_bandit, id.vars = 'batch')
gg_df_stat <- melt(pmat_stat, id.vars = 'batch')


gg_df <- bind_rows(
  list(
    Adaptive = gg_df,
    Static = gg_df_stat),
  .id = 'type'
)

ylim <- c(0, max(gg_df$value) + .01)

gg_df <- gg_df %>%
  mutate(type = factor(type, levels = c('Adaptive', 'Static'))) %>% 
  group_by(batch, type) %>%
  mutate(p.order = rank(-value, ties.method = 'first'),
         profile = variable) %>%
  group_by(profile, type) %>%
  mutate(p.last = last(p.order),
         p.cat = cut(p.last,
                     breaks = breaks))

# put in previous mutate
levels(gg_df$p.cat) <- paste0('[', breaks[1:(length(breaks)-1)]+1, ',', 
                              breaks[2:length(breaks)], ']' )

g <-
  ggplot(gg_df,
         aes(
           x = batch,
           y = value,
           group = profile,
           color = p.cat,
           linetype = p.cat
         )) +
  geom_line() +
  facet_wrap(~type) +
  theme_bw() +
  theme(strip.background = element_blank()) +
  coord_cartesian(xlim = c(0, 10),
                  ylim = ylim,
                  clip = 'off') +
  scale_x_continuous(breaks = c(seq(1, 10, 1))) +
  scale_y_continuous(breaks = c(seq(0, .5, .1))) +
  scale_colour_manual(
    name = 'Position in last period',
    values = gray(seq(.1,.6, len = length(levels(gg_df$p.cat)) ))) +
  scale_linetype_manual(name = 'Position in last period',
                        values = 1:length(levels(gg_df$p.cat)) ) +
  ylab('Posterior probability of being the best arm') +
  xlab('Batch number') 

g

# ggsave(
#   filename = 'figureG16.pdf',
#   plot = g,
#   width = 6.5,
#   height = 4
# )


# * Factors faceted ----
# ** [Figure G15] ----
dd <- bind_rows(
  list(Static = bind_cols(Z_combs[, c('Z_1', 'Z_2', 'Z_3', 'Z_4')], 
                          as.data.frame(t(pmat_stat[,-1])) ),
       Adaptive = bind_cols(Z_combs[, c('Z_1', 'Z_2', 'Z_3', 'Z_4')], 
                            as.data.frame(t(pmat_bandit[,-1])) )),
  .id = 'type')

names(dd)[6:16] <- 0:10


factor_l <- c('Personal limits', 'Corporate limits', 'Public funding',
              'Disclosures')

gg_df <- bind_rows( 
  dd %>% 
    group_by(type, Z_1) %>% 
    summarise_if(is.numeric, sum) %>% 
    rename(profile = Z_1),
  dd %>% 
    group_by(type, Z_2) %>% 
    summarise_if(is.numeric, sum) %>% 
    rename(profile = Z_2),
  dd %>% 
    group_by(type, Z_3) %>% 
    summarise_if(is.numeric, sum) %>% 
    rename(profile = Z_3),
  dd %>% 
    group_by(type, Z_4) %>% 
    summarise_if(is.numeric, sum) %>% 
    rename(profile = Z_4)
) %>% 
  melt() %>% 
  mutate(type = factor(type, levels = c('Adaptive', 'Static')),
         batch = (as.integer(variable)-1),
         z = factor(rep(rep(
           factor_l, 
           c(8, 6, 8, 8)), nrow(pmat_bandit)),
           levels = factor_l) )  %>% 
  group_by(batch, z, type) %>%
  mutate(p.cat = rank(-value, ties.method = 'first')) %>%
  ungroup() %>%
  group_by(z, profile, type) %>% 
  mutate(p.cat = p.cat[batch == 10]) %>% 
  ungroup() %>% 
  mutate(p.cat = factor(p.cat))


text_df <- filter(gg_df, batch == max(batch))

g <-
  ggplot( gg_df,
          aes(
            x = batch,
            y = value,
            group = profile,
            color = p.cat
          )) +
  geom_line() +
  theme_bw() +
  facet_grid(z~type) +
  coord_cartesian(xlim = c(0, 15),
                  ylim = c(0, max(gg_df$value) + .1),
                  clip = 'off') +
  scale_x_continuous(breaks = c(seq(1, 10, 1) )) +
  scale_y_continuous(breaks = c(seq(0, 1, 0.25) )) +
  scale_colour_manual(
    name = 'Position in last period',
    values = gray(seq(.1,.6, len = length(levels(gg_df$p.cat)) ))) +
  scale_linetype_manual(name = 'Position in last period',
                        values = 1:length(levels(gg_df$p.cat)) ) +
  theme(legend.position = 'none', strip.background = element_blank(),
        panel.grid.minor = element_blank()) +
  ylab('Posterior probability of being the best arm') +
  xlab('Batch number') +
  geom_text_repel(
    data = text_df,
    aes(label = profile),
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
#   filename = 'figureG15.pdf',
#   plot = g,
#   width = 6.5,
#   height = 6.5
# )


# ggsave(
#   filename = 'figureG15_color.pdf',
#   plot = g + scale_color_discrete(),
#   width = 6.5,
#   height = 6.5
# )



# Other reported results ----
# Top three profiles in bandit experiment
# posterior probabilities
round(head(sort(unlist(pmat_bandit[11, 2:193]), decreasing = TRUE), 3), 3)
# profile content
Z_combs[names(head(sort(unlist(pmat_bandit[11, 2:193]), decreasing = TRUE), 3)), 
   c('Z_1', 'Z_2', 'Z_3', 'Z_4')]

# Top three profiles in static experiment
# posterior probabilities
round(head(sort(unlist(pmat_stat[11, 2:193]), decreasing = TRUE), 3), 3)
# profile content
Z_combs[names(head(sort(unlist(pmat_stat[11, 2:193]), decreasing = TRUE), 3)), 
   c('Z_1', 'Z_2', 'Z_3', 'Z_4')]

