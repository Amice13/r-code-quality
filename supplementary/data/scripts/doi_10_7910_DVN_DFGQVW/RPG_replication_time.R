rm(list=setdiff(ls(), 'theme.plot'))

source('RPG_replication_aux_functions.R')

#Load the data:
# Parsed, scraped data from the Diet API: See SI for discussion.
store.markov <- readRDS('./final_data/markov_output.RDS')
store.markov$committee.type <- factor(store.markov$committee.type, 
                                      levels = c('Budget', 'Standing', 'Other'))

# Collapse data by "scheme.id = 1" (i.e. periodization 1; see SI)
regression.markov <- collapse.data.by.scheme(data = store.markov, outcome = count,
                                             scheme.id = 1, limit.scheme = TRUE)

#Confirm that all sums to one (i.e. value/total is *proportion* of first speeches)
regression.markov %>% 
  group_by(committee,chamber,first_speaker,cabinet_number) %>% 
  summarize(sum = sum(value/total)) %>%
  pull(sum) %>% table

#Confirm that all sums to one for TOTAL speeches.
regression.markov %>% 
  group_by(committee,chamber,cabinet_number) %>% 
  summarize(sum = sum(value/total.committee)) %>%
  pull(sum) %>% table

# Get speakers of interest for plots (i.e. not "other" or "unclear")
# See SI for discussion
spkrs <- setdiff(unique(regression.markov$first_speaker), 
                 c('other', 'unclear'))
print(spkrs)

#Lower (HOR) and Upper Chamber (HOC), Respectively.
chamber <- c('syugiin', 'sangiin')

###########################################
# Regressions for activity 
# (Pr of Speeches by Type)
###########################################

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

scheme.1 <- periodize.data(scheme.id = 1)
#Last period of the pre-reform period
baseline.scheme <- scheme.1$period %>% filter(period == 'Reform 1') %>% pull(cabinet_number) %>% min(.) -1
baseline.scheme <- as.character(baseline.scheme)

time.activity.regressions <- activity.data %>% filter(period != 'Pre-1955') %>% 
  group_by(chamber, first_speaker) %>% 
  do(
    fmt.regression(
      plm(formula = pr.activity ~ relevel(factor(cabinet_number), ref = baseline.scheme), data = ., index = 'committee'), 
      se.type = 'robust')
  )
time.discursive.regressions <- discursive.data %>% filter(period != 'Pre-1955') %>% group_by(chamber, first_speaker, second_speaker) %>% 
  do(
    fmt.regression(
      plm(formula = pr.discursive ~ relevel(factor(cabinet_number), ref = baseline.scheme), data = ., index = 'committee'), 
      se.type = 'robust')
  )

time.activity.regressions <- clean.columns(time.activity.regressions)
time.activity.regressions$cabinet_number <- as.numeric(gsub(time.activity.regressions$variable, 
                                                            pattern='[^0-9]', replacement = ''))
time.activity.regressions$fmt_period <- scheme.1$period$period[match(time.activity.regressions$cabinet_number, scheme.1$period$cabinet_number)]
time.activity.regressions$fmt_period <- ifelse(time.activity.regressions$fmt_period == '1955 System', 'Pre-Reform', time.activity.regressions$fmt_period)

time.discursive.regressions <- clean.columns(time.discursive.regressions)
time.discursive.regressions$cabinet_number <- as.numeric(gsub(time.discursive.regressions$variable, 
                                                              pattern='[^0-9]', replacement = ''))
time.discursive.regressions$fmt_period <- scheme.1$period$period[match(time.discursive.regressions$cabinet_number, scheme.1$period$cabinet_number)]

time.discursive.regressions$fmt_period <- ifelse(time.discursive.regressions$fmt_period == '1955 System', 'Pre-Reform', time.discursive.regressions$fmt_period)

g.time.pr <- ggplot(time.activity.regressions) + geom_point(aes(x=cabinet_number,y=est, col = fmt_period)) + facet_grid(fmt.chamber ~ fmt_first_speaker) +
  geom_errorbar(aes(x=cabinet_number,ymin=est-1.96*se,ymax=est+1.96*se, col = fmt_period)) +
  geom_hline(aes(yintercept=0), col = 'red', linetype = 'dashed') + xlab('Cabinet Number') + ylab('Change in Proportion') + theme_bw() +
  geom_point(aes(x=as.numeric(baseline.scheme), y=0)) +
  labs(col = 'Time Period:') + theme(legend.position = 'bottom')

ggsave(filename = 'figures/fig_a4_time_FE_reg_pr.pdf', plot = g.time.pr,
       width = 8, height = 6)

g.time.pr.da <- ggplot(time.discursive.regressions %>% filter(first_speaker == 'opposition'  & !grepl(fmt.name, pattern='Intercept')),
       aes(x = cabinet_number, y= est, ymin = est-1.96*se, ymax=est+1.96*se, group=fmt.chamber, col = fmt_period)
) + geom_point() + geom_errorbar() + facet_grid(fmt.chamber ~ fmt_second_speaker) +
  geom_hline(aes(yintercept=0), col = 'red', linetype = 'dashed') + xlab('Cabinet Number') + ylab('Change in Proportion') + theme_bw() +
  geom_point(aes(x=as.numeric(baseline.scheme), y=0)) +
  labs(col = 'Time Period:') + theme(legend.position = 'bottom')

ggsave(filename = 'figures/fig_a5_time_FE_da_reg_pr.pdf', plot = g.time.pr.da,
       width = 8, height = 6)
