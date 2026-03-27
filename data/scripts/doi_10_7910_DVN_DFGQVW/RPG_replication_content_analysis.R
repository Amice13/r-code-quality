rm(list=setdiff(ls(), 'theme.plot'))

source('RPG_replication_aux_functions.R')

#Load the data:
# Parsed, scraped data from the Diet API: See SI for discussion.
store.markov <- readRDS('./final_data/markov_output.RDS')
store.markov$committee.type <- factor(store.markov$committee.type, 
                                      levels = c('Budget', 'Standing', 'Other'))

spkrs <- setdiff(unique(store.markov$first_speaker), 
                 c('other', 'unclear'))
print(spkrs)

regression.markov <- collapse.data.by.scheme(data = store.markov,
     outcome = count,
    scheme.id = 1, limit.scheme = TRUE)
activity.data  <- regression.markov %>% group_by(chamber, committee, cabinet, cabinet_number,  
                                                 budget, period, committee.type, first_speaker) %>% 
  #Get total number of speeches by first_speaker.
  summarize(value = sum(value), total.committee = unique(total.committee)) %>%
  #Get proportion of activity by type.
  mutate(pr.activity = value/total.committee)
activity.data <- activity.data %>% filter(first_speaker %in% spkrs)


all.qd <- all.trend <- store_ques_activity <- store_questions <- data.frame()
for (outcome in c('count', 'has_question', 'has_responsibility', 'has_minister')){
  message(outcome)
  #Set temporary variable
  store.markov$temp <- store.markov[[gsub(outcome, pattern='^not_', replacement = '')]]
  if (grepl(outcome, pattern = 'not')){
    store.markov$temp <- store.markov$count - store.markov$temp
  }
  question.markov <- collapse.data.by.scheme(data = store.markov, 
                          outcome = temp, scheme.id = 1, limit.scheme = TRUE)
  
  ques.act.data  <- question.markov %>% group_by(chamber, committee, cabinet, cabinet_number,  
                                                 budget, period, committee.type, first_speaker) %>% 
    #Get total number of speeches by first_speaker.
    summarize(value = sum(value), total.committee = unique(total.committee)) %>%
    #Get proportion of activity by type.
    mutate(pr.activity = value/total.committee)
  
  ques.act.data <- full_join(activity.data %>% 
                               select(chamber, committee, cabinet, cabinet_number, period, first_speaker,
                                      all_count = value),
                             ques.act.data %>% filter(first_speaker %in% spkrs) %>% select(chamber, committee, cabinet, cabinet_number, period, first_speaker,
                                                                                           subset_count = value))
  ques.act.data <- ques.act.data %>% mutate(pr.subset = subset_count/all_count)
  
  trend.act.data <- ques.act.data %>% group_by(chamber, first_speaker, period) %>% summarize(pr.subset = sum(subset_count)/sum(all_count))
  all.trend <- bind_rows(all.trend, trend.act.data %>% mutate(type = outcome))
  
  ques.pr.reg <- ques.act.data %>% filter(period != 'Pre-1955') %>% group_by(chamber, first_speaker) %>% 
    do(
      fmt.regression(
        plm(formula = pr.subset ~ period, data = ., index = 'committee'), 
        se.type = 'robust')
    )
  
  store_ques_activity <- bind_rows(store_ques_activity, ques.pr.reg %>% mutate(outcome = outcome))
  
  question.discursive  <- question.markov %>% filter(first_speaker %in% spkrs & second_speaker %in% spkrs)
  question.discursive <- question.discursive %>% mutate(pr.discursive = value/total)
  
  qd.act.data <- question.discursive %>% group_by(chamber, cabinet_number, first_speaker, second_speaker, period) %>% summarize(pr.discursive = sum(value)/sum(total))
  all.qd <- bind_rows(all.qd, qd.act.data %>% mutate(type = outcome))
  
  question.discrusive.reg <- question.discursive %>% filter(period != 'Pre-1955') %>% group_by(chamber, first_speaker, second_speaker) %>% 
    do(
      fmt.regression(
        plm(formula = pr.discursive ~ (period), data = ., index = 'committee'), 
        se.type = 'robust')
    )
  
  store_questions <- bind_rows(store_questions, 
                               question.discrusive.reg %>% mutate(outcome = outcome))
  
}

store_questions <- clean.columns(store_questions)
store_questions$fmt.outcome <- c('All', 'Question', 'Responsibility', 'Minister')[match(store_questions$outcome, c('count', 'has_question', 'has_responsibility', 'has_minister'))]
store_questions$fmt.outcome <- factor(store_questions$fmt.outcome, levels = c('All', 'Question', 'Responsibility', 'Minister'))

store_ques_activity <- clean.columns(store_ques_activity)
store_ques_activity$fmt.outcome <- c('All', 'Question', 'Responsibility', 'Minister')[match(store_ques_activity$outcome, c('count', 'has_question', 'has_responsibility', 'has_minister'))]
store_ques_activity$fmt.outcome <- factor(store_ques_activity$fmt.outcome, levels = c('All', 'Question', 'Responsibility', 'Minister'))

g_questions_hor <- ggplot(store_questions %>% filter(
  chamber == 'syugiin' & first_speaker == 'opposition' & !grepl(outcome, pattern='not')),
  aes(x=fmt.name, y = est, ymin = est-1.96*se,ymax=est+1.96*se, col=fmt.outcome,group = fmt.outcome, pch = fmt.outcome)) +
  geom_hline(aes(yintercept=0), col = 'red') +
  geom_errorbar(position = position_dodge(0.5), width = 0.5) +
  geom_point(position = position_dodge(0.5)) + 
  scale_shape_manual(values = c(16, 17, 18, 19)) +
  scale_color_manual(values = c('black', 'red', 'blue', 'purple')) +
  facet_wrap(. ~ fmt_second_speaker) +
  theme_bw() + 
  labs(pch = 'Outcome:', col = 'Outcome:') +
  ylab('Estimate') + xlab('Time Period') +
  theme(legend.position = 'bottom', text = element_text(size = 12)) +
  ggtitle('(a) House of Representatives')

g_questions_hoc <- ggplot(store_questions %>% filter(
  chamber == 'sangiin' & first_speaker == 'opposition' & !grepl(outcome, pattern='not')),
  aes(x=fmt.name, y = est, ymin = est-1.96*se,ymax=est+1.96*se, col = fmt.outcome, group = fmt.outcome, pch = fmt.outcome)) +
  geom_hline(aes(yintercept=0), col = 'red') +
  geom_errorbar(position = position_dodge(0.5), width = 0.5) +
  geom_point(position = position_dodge(0.5)) + 
  scale_shape_manual(values = c(16, 17, 18, 19)) +
  scale_color_manual(values = c('black', 'red', 'blue', 'purple')) +
  facet_wrap(. ~ fmt_second_speaker) +
  theme_bw() + 
  labs(pch = 'Outcome:', col = 'Outcome:') +
  ylab('Estimate') + xlab('Time Period') +
  theme(legend.position = 'bottom', text = element_text(size = 12)) +
  ggtitle('(b) House of Councillors')

g_questions_both <- ggpubr::ggarrange(g_questions_hor, g_questions_hoc, common.legend = TRUE, 
                                      legend = 'bottom')
ggsave(filename = 'figures/fig_a12_questions_da.pdf', g_questions_both, width = 8, height = 8.5/2)


g_ques_a_hor <- ggplot(store_ques_activity %>% filter(
  chamber == 'syugiin' & !grepl(outcome, pattern='not')),
  aes(x=fmt.name, y = est, ymin = est-1.96*se,ymax=est+1.96*se, col=fmt.outcome,group = fmt.outcome, pch = fmt.outcome)) +
  geom_hline(aes(yintercept=0), col = 'red') +
  geom_errorbar(position = position_dodge(0.5), width = 0.5) +
  geom_point(position = position_dodge(0.5)) + 
  scale_shape_manual(values = c(16, 17, 18, 19)) +
  scale_color_manual(values = c('black', 'red', 'blue', 'purple')) +
  facet_wrap(. ~ fmt_first_speaker) +
  theme_bw() +
  labs(pch = 'Outcome:', col = 'Outcome:') +
  ylab('Estimate') + xlab('Time Period') +
  theme(legend.position = 'bottom', text = element_text(size = 12)) +
  ggtitle('(a) House of Representatives')

g_ques_a_hoc <- ggplot(store_ques_activity %>% filter(
  chamber == 'sangiin' &  !grepl(outcome, pattern='not')),
  aes(x=fmt.name, y = est, ymin = est-1.96*se,ymax=est+1.96*se, col=fmt.outcome,group = fmt.outcome, pch = fmt.outcome)) +
  geom_hline(aes(yintercept=0), col = 'red') +
  geom_errorbar(position = position_dodge(0.5), width = 0.5) +
  geom_point(position = position_dodge(0.5)) + 
  scale_shape_manual(values = c(16, 17, 18, 19)) +
  scale_color_manual(values = c('black', 'red', 'blue', 'purple')) +
  facet_wrap(. ~ fmt_first_speaker) +
  theme_bw() + 
  labs(pch = 'Outcome:', col = 'Outcome:') +
  ylab('Estimate') + xlab('Time Period') +
  theme(legend.position = 'bottom', text = element_text(size = 12)) +
  ggtitle('(b) House of Councillors')

g_qua_both <- ggpubr::ggarrange(g_ques_a_hor, g_ques_a_hoc, common.legend = TRUE, 
                                legend = 'bottom')
print(g_qua_both)

store_ques_activity %>% filter(
  !grepl(outcome, pattern='not')
) %>% filter(first_speaker == 'opposition') %>%
  dcast(outcome + chamber ~ variable, value.var = 't')

store_questions %>% filter(
  !grepl(outcome, pattern='not')
) %>% filter(first_speaker == 'opposition') %>%
  dcast(outcome + second_speaker + chamber ~ variable, value.var = 't') %>%
  filter(outcome %in% c('has_question', 'has_responsibility')) %>%
  filter(second_speaker %in% c('minister', 'junior.minister', 'bureaucrat'))


all.trend <- clean.columns(all.trend)
all.trend$fmt.outcome <- c('All', 'Question', 'Responsibility', 'Minister')[match(all.trend$type, c('count', 'has_question', 'has_responsibility', 'has_minister'))]
all.trend$fmt.outcome <- factor(all.trend$fmt.outcome, levels = c('All', 'Question', 'Responsibility', 'Minister'))


all.trend <- all.trend %>%
  mutate(short_chamber = recode_factor(chamber, 'syugiin' = 'HR', 'sangiin' = 'HC'))
g_trend <- ggplot(all.trend %>% filter(type != 'count' & period != 'Pre-1955')) +
  geom_bar(aes(x=short_chamber,y=pr.subset,
               # col = fmt.chamber,
               fill=factor(fmt_period), group=interaction(fmt.chamber, fmt_period)),
           stat = 'identity', width = 1/2, position = position_dodge(width = 1/2)) +
  facet_grid(fmt.outcome ~ fmt_first_speaker, scale = 'free_y') +
  theme_bw() + labs(fill = 'Period: ') +
  scale_fill_grey(start = 0.8, end = 0) +
  theme(legend.position = 'bottom',
        # axis.text.x = element_text(color = c('red', 'blue'))
  ) +
  # scale_color_manual(values = c('red', 'blue'), guide = FALSE) +
  xlab('Period') + ylab('Proportion of Speech Containing Phrase')

ggsave(plot = g_trend,
       filename = 'figures/fig_a11_content_bar_vis.pdf',
       width = 8, height = 6)

