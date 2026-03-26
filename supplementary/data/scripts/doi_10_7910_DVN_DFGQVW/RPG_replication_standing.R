rm(list=setdiff(ls(), 'theme.plot'))

source('RPG_replication_aux_functions.R')
store.markov <- readRDS('./final_data/markov_output.RDS')
store.markov$committee.type <- factor(store.markov$committee.type, 
                                      levels = c('Budget', 'Standing', 'Other'))

spkrs <- setdiff(unique(store.markov$first_speaker), 
                 c('other', 'unclear'))
print(spkrs)

#Load the data:
# Parsed, scraped data from the Diet API: See SI for discussion.
store.markov <- readRDS('./final_data/markov_output.RDS')
store.markov$committee.type <- factor(store.markov$committee.type, 
                                      levels = c('Budget', 'Standing', 'Other'))

# Collapse data by "scheme.id = 1" (i.e. periodization 1; see SI)
regression.markov <- collapse.data.by.scheme(data = store.markov, outcome = count, 
                                             scheme.id = 1, limit.scheme = TRUE)

activity.data  <- regression.markov %>% 
  group_by(chamber, committee, cabinet, cabinet_number, 
           budget, period, committee.type, first_speaker) %>% 
  #Get total number of speeches by first_speaker.
  summarize(value = sum(value), total.committee = unique(total.committee)) %>%
  #Get proportion of activity by type.
  mutate(pr.activity = value/total.committee)
activity.data <- activity.data %>% filter(first_speaker %in% spkrs)

discursive.data  <- regression.markov %>% 
  filter(first_speaker %in% spkrs & second_speaker %in% spkrs)

discursive.data <- discursive.data %>% 
  mutate(pr.discursive = value/total)


# Loop over schemes/periodization
# Takes a while to run but shouldn't be too intensive
# on a modern laptop

spkrs <- setdiff(unique(store.markov$first_speaker), 
                 c('other', 'unclear'))
print(spkrs)

# Examine the (standing) committees that do not appear in all three periods
rare_committees <- activity.data %>% filter(period != 'Pre-1955' & committee.type != 'Other') %>% 
  group_by(chamber, committee.type, committee) %>% 
  summarize(u_period = length(unique(period))) %>%
  filter(u_period != 3)

print(rare_committees)

# Standing committee by standing committee regressions
activity.regressions.by.standing <- activity.data %>% 
  filter(!(committee %in% c('Oversight', 'National Fundamental Policy'))) %>%
  filter(period != 'Pre-1955' & committee.type != 'Other') %>% 
  group_by(chamber, committee, committee.type, first_speaker) %>% 
  do(
    fmt.regression(
      lm(formula = pr.activity ~ period, data = .), 
      se.type = 'robust')
  )

activity.regressions.by.standing <- clean.columns(activity.regressions.by.standing)
activity.regressions.by.standing$committee <- gsub(activity.regressions.by.standing$committee, pattern='\\(.*\\)', replacement = '')

activity.regressions.by.standing$committee <- str_trim(activity.regressions.by.standing$committee)

activity.regressions.by.standing$committee <- factor(activity.regressions.by.standing$committee,
                                                     levels = rev(sort(unique(activity.regressions.by.standing$committee))), ordered = TRUE)

# Figure A.7
g.pr.standing <- ggplot(activity.regressions.by.standing %>% filter(first_speaker %in% c('bureaucrat', 'minister', 'junior.minister') &
                                                     !grepl(variable, pattern='Intercept')),
       aes(y=est, ymin=est-1.96*se, ymax=est+1.96*se, x=committee, pch = fmt.name, linetype = fmt.name, col = fmt.name, group = interaction(variable))) + 
  geom_point(position = position_dodge(width = 1)) +
  geom_errorbar(position = position_dodge(width=1)) +
  geom_hline(aes(yintercept=0), linetype = 'dashed') + 
  scale_color_grey(start = 0, end = 0.4) + 
  facet_grid(fmt.chamber ~ fmt_first_speaker) + coord_flip() + theme_bw() +
  theme(legend.position = 'bottom') + ylab('Change in Proportion') + xlab('Committee') + 
  labs(col = 'Time Period:', linetype = 'Time Period:', pch = 'Time Period:')
ggsave(filename = 'figures/fig_a7_FE_reg_pr_standing.pdf', plot = g.pr.standing,
       width = 8, height = 8)

#Excluding missing data
discursive.regressions.by.standing <- discursive.data %>% 
  filter(!(committee %in% c('Oversight', 'National Fundamental Policy'))) %>%
  filter(period != 'Pre-1955' & committee.type != 'Other') %>%
  filter(first_speaker == 'opposition')

# Missingness from Discipline committee in HOC
discursive.regressions.by.standing %>% 
  group_by(committee, chamber, committee.type, 
           first_speaker, second_speaker) %>%
  summarize(non.missing = sum(!is.na(pr.discursive))) %>%
  filter(non.missing == 0) %>% data.frame %>%
  print

discursive.regressions.by.standing <- discursive.regressions.by.standing %>% filter(committee != 'Discipline')
discursive.regressions.by.standing <- discursive.regressions.by.standing %>% group_by(chamber, committee, committee.type, first_speaker, second_speaker) %>% 
  do(fmt.regression(
    lm(formula = pr.discursive ~ period, data = .), 
    se.type = 'robust')
  )

discursive.regressions.by.standing <- clean.columns(discursive.regressions.by.standing)
discursive.regressions.by.standing$committee <- gsub(discursive.regressions.by.standing$committee, pattern='\\(.*\\)', replacement = '')
discursive.regressions.by.standing$committee <- str_trim(discursive.regressions.by.standing$committee)

discursive.regressions.by.standing$committee <- factor(discursive.regressions.by.standing$committee,
                                                       levels = rev(sort(unique(discursive.regressions.by.standing$committee))), ordered = TRUE)

# Figure A.8
g.pr.standing.da <- ggplot(discursive.regressions.by.standing %>% 
         filter(second_speaker %in% c('bureaucrat', 'minister', 'junior.minister') & !grepl(variable, pattern='Intercept')),
       aes(y=est, ymin=est-1.96*se, ymax=est+1.96*se, x=committee, pch = fmt.name, linetype = fmt.name, col = fmt.name, group = interaction(variable))) + 
  geom_point(position = position_dodge(width = 1)) +
  geom_errorbar(position = position_dodge(width=1)) +
  geom_hline(aes(yintercept=0), linetype = 'dashed') + 
  scale_color_grey(start = 0, end = 0.4) + 
  facet_grid(fmt.chamber ~ fmt_second_speaker) + coord_flip() + theme_bw() +
  theme(legend.position = 'bottom') + ylab('Change in Proportion') + xlab('Committee') + 
  labs(col = 'Time Period:', linetype = 'Time Period:', pch = 'Time Period:')

ggsave(filename = 'figures/fig_a8_FE_da_reg_pr_standing.pdf', plot = g.pr.standing.da,
       width = 8, height = 8)
