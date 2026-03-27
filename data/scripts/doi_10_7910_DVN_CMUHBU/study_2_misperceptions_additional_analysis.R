## Adaptive Experimental Design: Prospects and Applications in Political Science
## Code for Study 2: misperceptions, additional analysis
## Produces Figure F12 and Table F11 

version

# install.packages(c('coefplot', 'stargazer'))

library(coefplot)
library(stargazer)


# Set options ----
source('utils.R')

# Additional analyses ----

mturk_bandit <- read_rds('study_2_clean.rds')

# * Attrition ----
# ** [Figure F12] ----
mturk_bandit <- mturk_bandit %>%
  mutate(econ1 = case_when(econ_followup_1 == 'Good' ~ 1,
                           econ_followup_1 == 'Very good' ~ 1,
                           is.na(econ_followup_1) ~ NA_real_,
                           TRUE ~ 0),
         econ2 = case_when(econ_followup_2 == 'get worse' ~ 0,
                           econ_followup_2 == 'stay about the same' ~ 0,
                           econ_followup_2 == 'get better' ~ 1),
         always_present = 1*(Y_no_response_sum==0))

# Examine how treatment affects attrition;
# 'always_present' is 1 if subject responded to all questions
fit_attrit_d <- lm_robust(always_present ~ Z, weights = weights, 
                          data = filter(mturk_bandit,
                                        pid == 'democrat'))

fit_attrit_r <- lm_robust(always_present ~ Z, weights = weights, 
                          data = filter(mturk_bandit,
                                        pid == 'republican'))

gg_df <-
  bind_rows(
    `Democrats` = tidy(fit_attrit_d),
    `Republicans` = tidy(fit_attrit_r),
    .id = 'type'
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

g <- 
  ggplot(gg_df, aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, 
                     xmax = conf.high), 
                 height = 0, 
                 position = position_dodgev(height = 0.4)) +
  geom_vline(xintercept = 0, 
             linetype = 'dashed') +
  facet_wrap(~ type) +
  theme_bw() +
  coord_cartesian(xlim=c(-.5, .1)) + 
  geom_text(aes(label = make_entry(gg_df$estimate, 
                                   gg_df$std.error, gg_df$p.value)),
            size = 2.5, 
            show.legend = FALSE, 
            nudge_y = .15) +
  theme(strip.background = element_blank(), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) 

g

# ggsave(
#   filename = 'figureF12.pdf',
#   plot = g,
#   width = 6.5,
#   height = 4
# )


# * Downstream analysis ----
# Because of differential attrition, we exclude Google and Extratime from the 
# following analysis. 
# ** [Table F11] ----

# outcome 1
fit_d1_direct <- lm(econ1 ~ Z, 
                    weights = weights, 
                    data = filter(mturk_bandit,
                                  Z != 'extratime',
                                  Z != 'google',  
                                  pid == 'democrat'))

fit_d1cv_direct <- lm(econ1 ~ Z
                      + follow_pol_pre + race_pre + educ_5_pre + 
                        female_pre + age_pre + pid_7_pre + ideo_7_pre, 
                      weights = weights, 
                      data = filter(mturk_bandit,
                                    Z != 'extratime',
                                    Z != 'google',  
                                    pid == 'democrat'))
fit_r1_direct <- lm(econ1 ~ Z, 
                    weights = weights, 
                    data = filter(mturk_bandit,
                                  Z != 'extratime',
                                  Z != 'google',  
                                  pid == 'republican'))

fit_r1cv_direct <- lm(econ1 ~ Z
                      + follow_pol_pre + race_pre + educ_5_pre + 
                        female_pre + age_pre + pid_7_pre + ideo_7_pre, 
                      weights = weights, 
                      data = filter(mturk_bandit,
                                    Z != 'extratime',
                                    Z != 'google',  
                                    pid == 'republican'))

fit_d2_direct <- lm(econ2 ~ Z, 
                    weights = weights, 
                    data = filter(mturk_bandit,
                                  Z != 'extratime',
                                  Z != 'google',  
                                  pid == 'democrat'))

fit_d2cv_direct <- lm(econ2 ~ Z
                      + follow_pol_pre + race_pre + educ_5_pre + 
                        female_pre + age_pre + pid_7_pre + ideo_7_pre, 
                      weights = weights, 
                      data = filter(mturk_bandit,
                                    Z != 'extratime',
                                    Z != 'google',  
                                    pid == 'democrat'))

fit_r2_direct <- lm(econ2 ~ Z, 
                    weights = weights, 
                    data = filter(mturk_bandit,
                                  Z != 'extratime',
                                  Z != 'google',  
                                  pid == 'republican'))

fit_r2cv_direct <- lm(econ2 ~ Z
                      + follow_pol_pre + race_pre + educ_5_pre + 
                        female_pre + age_pre + pid_7_pre + ideo_7_pre, 
                      weights = weights, 
                      data = filter(mturk_bandit,
                                    Z != 'extratime',
                                    Z != 'google',  
                                    pid == 'republican'))


# plot
# econ1, without covariates
stargazer(
  fit_d1_direct,
  fit_d1cv_direct,
  fit_r1_direct,
  fit_r1cv_direct,
  fit_d2_direct,
  fit_d2cv_direct,
  fit_r2_direct,
  fit_r2cv_direct,
  se = list(
    starprep(fit_d1_direct)[[1]],
    starprep(fit_d1cv_direct)[[1]],
    starprep(fit_r1_direct)[[1]],
    starprep(fit_r1cv_direct)[[1]],
    starprep(fit_d2_direct)[[1]],
    starprep(fit_d2cv_direct)[[1]],
    starprep(fit_r2_direct)[[1]],
    starprep(fit_r2cv_direct)[[1]]
  ),
  keep = c('Zlottery', 'Zaccuracy', 'Zdirection'), 
  covariate.labels = c('Lottery', 'Accuracy', 'Direction'),
  float = FALSE,
  model.names = FALSE,
  column.labels = rep(c('Democrats', 'Republicans'), 2),
  column.separate = c(2, 2, 2, 2),
  dep.var.labels = c('Follow-up 1', 'Follow-up 2'),
  omit.table.layout = 'n',
  omit.stat = c('ser', 'f', 'adj.rsq'),
  header = FALSE,
  add.lines = list(c('Covariate adjusted', rep(c('No', 'Yes'), 4))),
  star.char = c("*"),
  star.cutoffs = c(0.05)#,
  #out = 'tableF11.tex'
  )
