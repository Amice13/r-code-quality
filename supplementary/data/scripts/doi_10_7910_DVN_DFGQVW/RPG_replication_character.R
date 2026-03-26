rm(list=setdiff(ls(), 'theme.plot'))

source('RPG_replication_aux_functions.R')

store.markov <- readRDS('./final_data/markov_output.RDS')
store.markov$committee.type <- factor(store.markov$committee.type, 
                                      levels = c('Budget', 'Standing', 'Other'))

char.regression.markov <- collapse.data.by.scheme(data = store.markov, 
                  outcome = characters, scheme.id = 1, limit.scheme = TRUE)

char.activity.data  <- char.regression.markov %>% group_by(chamber, committee, cabinet, cabinet_number,  
  budget, period, committee.type, first_speaker) %>% 
  #Get total number of speeches by first_speaker.
  summarize(value = sum(value), total.committee = unique(total.committee)) %>%
  #Get proportion of activity by type.
  mutate(pr.activity = value/total.committee)

spkrs <- setdiff(unique(char.regression.markov$first_speaker), 
                 c('other', 'unclear'))
print(spkrs)


char.activity.data <- char.activity.data %>% filter(first_speaker %in% spkrs)

char.activity.regressions <- char.activity.data %>% filter(period != 'Pre-1955') %>% group_by(chamber, first_speaker) %>% 
  do(
    fmt.regression(
      plm(formula = pr.activity ~ period, data = ., index = 'committee'), 
      se.type = 'robust')
  )

char.discursive.data  <- char.regression.markov %>% filter(first_speaker %in% spkrs & second_speaker %in% spkrs)
char.discursive.data <- char.discursive.data %>% mutate(pr.discursive = value/total)

char.discursive.regressions <- char.discursive.data %>% filter(period != 'Pre-1955') %>% group_by(chamber, first_speaker, second_speaker) %>% 
  do(
    fmt.regression(
      plm(formula = pr.discursive ~ (period), data = ., index = 'committee'), 
      se.type = 'robust')
  )

char.activity.regressions <- clean.columns(char.activity.regressions)
char.discursive.regressions <- clean.columns(char.discursive.regressions)

g.char <- ggplot(char.activity.regressions, 
       aes(x=fmt.name,y=est,ymin=est-1.96*se,ymax=est+1.96*se, 
           linetype = fmt.chamber, pch = fmt.chamber,
           group=fmt.chamber)
) + theme.plot + facet_wrap(. ~ fmt_first_speaker, nrow = 2) +
  ylim(c(-0.2, .2)) + ggtitle('(a) Activity of Groups')


g.char.da <- ggplot(char.discursive.regressions %>% filter(first_speaker == 'opposition'  & !grepl(fmt.name, pattern='Intercept')),
       aes(x = fmt.name, y= est, ymin = est-1.96*se, ymax=est+1.96*se, group=fmt.chamber,
           linetype = fmt.chamber, pch = fmt.chamber)) +
  theme.plot + facet_wrap(. ~ fmt_second_speaker, nrow = 2) +
  ylim(c(-0.35, 0.35)) + ggtitle('(b) Discursive Accountability')

g.char.both <- ggpubr::ggarrange(g.char, g.char.da, nrow = 1, common.legend = T,
                                 legend = 'bottom')

ggsave(filename = 'figures/fig_a3_char_FE_reg.pdf', plot = g.char.both,
       width = 8.5, height = 8.5/2)
