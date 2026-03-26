rm(list=setdiff(ls(), 'theme.plot'))

source('RPG_replication_aux_functions.R')

#Load the data:
# Parsed, scraped data from the Diet API: See SI for discussion.
store.markov <- readRDS('./final_data/markov_output.RDS')
store.markov$committee.type <- factor(store.markov$committee.type, 
  levels = c('Budget', 'Standing', 'Other'))

# Collapse data by "scheme.id = 1" (i.e. periodization 1; see SI)
regression.markov <- collapse.data.by.scheme(data = store.markov, 
    outcome = count,
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

# Descriptives for main text
examined_data <- activity.data %>% filter(period != 'Pre-1955') %>%
  group_by(chamber) %>% 
  summarize(n_committee = length(unique(committee)), total = sum(value))

print(examined_data)
print(sum(examined_data$total))
print(sum(activity.data$value))

# Create some simple descriptive figures (Figure 1/A.1)
plot.by.type <- activity.data %>% group_by(committee.type, period, first_speaker) %>% 
  summarize(avg.speaking = mean(value/total.committee))

plot.by.type <- clean.columns(plot.by.type)
da.plot.by.type <- discursive.data %>% group_by(committee.type, period, first_speaker, second_speaker) %>% 
  summarize(avg.speaking = mean(value/total, na.rm=T)) %>% filter(first_speaker == 'opposition')
da.plot.by.type <- clean.columns(da.plot.by.type)

bar.trends <- activity.data %>% group_by(first_speaker, chamber, cabinet, period, cabinet_number) %>% 
  summarize(count = sum(value)) %>% group_by(cabinet, chamber, period, cabinet_number) %>%
  mutate(total = sum(count)) %>% ungroup()
bar.trends <- clean.columns(bar.trends)

delim_period <- bar.trends %>% group_by(period) %>% summarize(min = min(cabinet_number)) %>%
  filter(grepl(period, pattern='Reform')) %>% pull(min)
delim_period <- delim_period - 0.5

zoomed.periods <- list('Pre-Reform' = 44:58,
                       'R1 Passage' = 59:63, 'R1 Enactment' = 64:66,
                       'R2 Passage' = 67:71, 'R2 Enactment' = 72:96)
zoomed.names <- names(zoomed.periods)
zoomed.periods <- bind_rows(lapply(zoomed.periods, data.frame), .id = 'zoomed.period')
zoomed.periods$zoomed.period <- as.character(zoomed.periods$zoomed.period)
names(zoomed.periods)[2] <- 'cabinet_number'

bar.trends$fmt.zoomed <- zoomed.periods$zoomed.period[match(bar.trends$cabinet_number, zoomed.periods$cabinet_number)]
bar.trends$fmt.zoomed <- factor(bar.trends$fmt.zoomed, levels = zoomed.names)


g.det.HOC <- ggplot(bar.trends %>% filter(period != 'Pre-1955' & fmt.chamber %in% c('House of Councillors') & first_speaker %in% spkrs)) + 
  geom_point(aes(x=cabinet_number, y=count/total, pch=fmt.zoomed), stat = 'identity') + 
  facet_wrap(~ fmt_first_speaker) +
  theme_bw() +
  ylab('Proportion of Speeches') + xlab('Cabinet Number') + labs(pch = 'Period:', col = 'Period:') +
  theme(legend.position = 'bottom', legend.box = 'vertical') + geom_vline(aes(xintercept=d), data = data.frame(d = delim_period), linetype = 'dashed') +
  scale_color_brewer(palette = 'Dark2') +
  guides(color = guide_legend(nrow = 2, byrow = T), pch = guide_legend(nrow = 2, byrow = T)) +
  ggtitle('House of Councillors')

g.det.HOR <- ggplot(bar.trends %>% filter(period != 'Pre-1955' & fmt.chamber %in% c('House of Representatives') & first_speaker %in% spkrs)) + 
  geom_point(aes(x=cabinet_number, y=count/total, pch=fmt.zoomed), stat = 'identity') + 
  facet_wrap(~ fmt_first_speaker) +
  theme_bw() +
  ylab('Proportion of Speeches') + xlab('Cabinet Number') + labs(pch = 'Period:', col = 'Period:') +
  theme(legend.position = 'bottom', legend.box = 'vertical') + geom_vline(aes(xintercept=d), data = data.frame(d = delim_period), linetype = 'dashed') +
  scale_color_brewer(palette = 'Dark2') +
  guides(color = guide_legend(nrow = 2, byrow = T), pch = guide_legend(nrow = 2, byrow = T)) 

# Figure 1
ggsave('figures/fig_1_detailed_HOR.pdf', g.det.HOR, width = 8.5, height = 5)

g.det.both <- ggpubr::ggarrange(g.det.HOR + ggtitle('House of Representatives'), g.det.HOC, ncol = 1, common.legend = T, legend = 'bottom')
# Figure A.1
ggsave('figures/fig_a1_detailed_bicameral.pdf',
       g.det.both, width = 8.5, height = 8)

# Data for Figure 2a
activity.regressions <- activity.data %>% 
  #Remove too old data (i.e. before start of period of analysis)
  filter(period != 'Pre-1955') %>% 
  #By chamber, type, run FE regression and format
  group_by(chamber, first_speaker) %>% 
  do(
    fmt.regression(
      plm(formula = pr.activity ~ period, data = ., index = 'committee'), 
      se.type = 'robust')
  )

# Data for Figure 4a
activity.regressions.by.type <- activity.data %>% 
  filter(period != 'Pre-1955') %>% 
  #Same but group by type of committee
  group_by(chamber, committee.type, first_speaker) %>% 
  do(
    if (.[['committee.type']][1] != 'Budget'){
      fmt.regression(
        plm(formula = pr.activity ~ period, data = ., index = 'committee'), 
        se.type = 'robust')      
    }else{
      #No FE for budget committee (single committee)
      fmt.regression(
        lm(formula = pr.activity ~ period, data = .), 
        se.type = 'robust')
    }
  )



######################################
# Discursive Accountability Regressions
# Pr(Next = Type | Prior Speaker Type)
# Pr(Minister Follows | Opposition Speaking)
#######################################

# Data for Figure 2b

discursive.regressions <- discursive.data %>% 
  filter(period != 'Pre-1955') %>% 
  group_by(chamber, first_speaker, second_speaker) %>% 
  do(
    fmt.regression(
      plm(formula = pr.discursive ~ (period), data = ., index = 'committee'), 
      se.type = 'robust')
  )

# Data for Figure 4b

discursive.regressions.by.type <- discursive.data %>% filter(period != 'Pre-1955') %>% group_by(chamber, committee.type, first_speaker, second_speaker) %>% 
  do(
    if (.[['committee.type']][1] != 'Budget'){
      fmt.regression(
        plm(formula = pr.discursive ~ (period), data = ., index = 'committee'), 
        se.type = 'robust')      
    }else{
      fmt.regression(
        lm(formula = pr.discursive ~ (period), data = .), 
        se.type = 'robust')
    }
  )

# Rough netting out of bureaucrat decline and ministerial rise in R2
discursive.regressions %>%
  filter(chamber == 'syugiin' & first_speaker == 'opposition') %>%
  reshape2::dcast(variable ~ second_speaker, value.var = 'est') %>%
  transmute(variable, bureaucrat, 
            sum_all = bureaucrat + minister + junior.minister + gov.backbench +
              opposition + gov.backbench + committee.chair,
            sum_three = bureaucrat + minister + junior.minister) %>%
  print

# Clean up regression data to make Figure 2 and 4

activity.regressions <- clean.columns(activity.regressions)
discursive.regressions <- clean.columns(discursive.regressions)

activity.regressions.by.type <- clean.columns(activity.regressions.by.type)
discursive.regressions.by.type <- clean.columns(discursive.regressions.by.type)

# Print the coefficient values for reference in the budget committee
activity.regressions.by.type %>% filter(committee.type == 'Budget') %>%
  filter(first_speaker == 'minister')  %>% filter(!is.na(fmt.name)) %>%
  dcast(fmt.chamber ~ fmt.name, value.var = 'est') %>% print
discursive.regressions.by.type %>% filter(committee.type == 'Budget') %>%
  filter(first_speaker == 'opposition' & second_speaker == 'minister')  %>% filter(!is.na(fmt.name)) %>%
  dcast(fmt.chamber ~ fmt.name, value.var = 'est') %>% print

# Figure 2

# NOT significant
activity.regressions %>% 
  filter(pr > 0.05) %>% select(chamber, first_speaker, variable) %>%
  print
# NOT significant
discursive.regressions %>% filter(first_speaker == 'opposition') %>%
  filter(pr > 0.05) %>% select(chamber, first_speaker, variable) %>%
  print

g_reg_HOR <- ggplot(activity.regressions %>% filter(fmt.chamber == 'House of Representatives'), 
                    aes(x=fmt.name,y=est,ymin=est-1.96*se,ymax=est+1.96*se, 
                        group=fmt.chamber)
) + theme.plot + facet_wrap(. ~ fmt_first_speaker, nrow = 2) +
  ylim(c(-0.15, .15)) + ggtitle('(a) Activity of Groups')

g_reg_da_HOR <- ggplot(discursive.regressions %>% filter(fmt.chamber == 'House of Representatives') %>%
                         filter(first_speaker == 'opposition'  & !grepl(fmt.name, pattern='Intercept')),
                       aes(x = fmt.name, y= est, ymin = est-1.96*se, ymax=est+1.96*se, group=fmt.chamber)) +
  theme.plot + facet_wrap(. ~ fmt_second_speaker, nrow = 2) +
  ylim(c(-0.35, 0.35)) + ggtitle('(b) Discursive Accountability')

ggsave(filename = 'figures/fig_2_FE_hor_pr.pdf', 
    ggpubr::ggarrange(g_reg_HOR, g_reg_da_HOR, nrow = 1), 
    width = 8.5, height = 8.5/2)

# Figure 4
g_FE_reg_pr_type <- ggplot(activity.regressions.by.type %>% 
                             filter(fmt.chamber == 'House of Representatives') %>%
                             filter(first_speaker %in% c('bureaucrat', 'minister', 'junior.minister', 'opposition')) %>%
                             filter(!grepl(variable, pattern='Intercept')),
                           aes(x = fmt.name, y= est, ymin = est-1.96*se, ymax=est+1.96*se, group=fmt.chamber)
) + geom_point(position = position_dodge(width = 1)) + 
  geom_errorbar(position = position_dodge(width = 1)) +
  facet_grid(committee.type ~ fmt_first_speaker) + theme_bw() +
  scale_color_grey(start = 0, end = 0.4) + 
  geom_hline(aes(yintercept=0), col = 'black', linetype = 'dashed') + xlab('Time Period') + ylab('Change in Proportion') + 
  labs(pch = 'Chamber', col = 'Chamber', linetype = 'Chamber') + theme(legend.position = 'bottom')  +
  ggtitle('(a) Activity of Groups')

g_FE_da_reg_pr_type <- ggplot(discursive.regressions.by.type %>% 
                                filter(fmt.chamber == 'House of Representatives') %>%
                                filter(second_speaker %in% c('bureaucrat', 'minister', 'junior.minister', 'opposition')) %>%
                                filter(first_speaker == 'opposition'  & !grepl(variable, pattern='Intercept')),
                              aes(x = fmt.name, y= est, 
                                  ymin = est-1.96*se, ymax=est+1.96*se, group=fmt.chamber)
) + geom_point(position = position_dodge(width = 1)) + 
  geom_errorbar(position = position_dodge(width = 1)) +
  facet_grid(committee.type ~ fmt_second_speaker) + theme_bw() +
  scale_color_grey(start = 0, end = 0.4) + 
  geom_hline(aes(yintercept=0), col = 'black', linetype = 'dashed') + xlab('Time Period') + ylab('Change in Proportion') + 
  labs(pch = 'Chamber', col = 'Chamber', linetype = 'Chamber') + theme(legend.position = 'bottom') +
  ggtitle('(b) Discursive Accountability')
g_FE_type <- ggpubr::ggarrange(plotlist = list(g_FE_reg_pr_type, g_FE_da_reg_pr_type), nrow = 1, common.legend = T,
                               legend = 'bottom')
ggsave('figures/fig_4_FE_comb_type.pdf', g_FE_type, width = 8.5, height = 8.5/1.5)

# Figure A.2
g_reg_HOC <- ggplot(activity.regressions %>% 
                      filter(fmt.chamber == 'House of Councillors'), 
                    aes(x=fmt.name,y=est,ymin=est-1.96*se,
                        ymax=est+1.96*se, group=fmt.chamber)
) + theme.plot + facet_wrap(. ~ fmt_first_speaker, nrow = 2) +
  ylim(c(-0.15, .15)) + ggtitle('(a) Activity of Groups')

g_reg_da_HOC <- ggplot(discursive.regressions %>% filter(fmt.chamber == 'House of Councillors') %>%
                         filter(first_speaker == 'opposition'  & !grepl(fmt.name, pattern='Intercept')),
                       aes(x = fmt.name, y= est, ymin = est-1.96*se, ymax=est+1.96*se, group=fmt.chamber)) +
  theme.plot + facet_wrap(. ~ fmt_second_speaker, nrow = 2) +
  ylim(c(-0.35, 0.35)) + ggtitle('(b) Discursive Accountability')

ggsave(filename = 'figures/fig_a2_FE_hoc_pr.pdf',
       ggpubr::ggarrange(g_reg_HOC, g_reg_da_HOC, nrow = 1), 
       width = 8.5, height = 8.5/2)

# Figure A.6 (a)

# For main text
plot.by.type %>% filter(period != 'Pre-1955' & 
  committee.type %in% c('Standing', 'Budget') &
  first_speaker %in% c('minister', 'bureaucrat')) %>% 
  reshape2::dcast(committee.type + fmt_period ~ first_speaker, value.var = 'avg.speaking')
plot.by.type %>% filter(period != 'Pre-1955' & 
  committee.type %in% c('Standing') &
  first_speaker %in% c('minister')) %>% print

g.type.pr <- ggplot(plot.by.type %>% filter(period != 'Pre-1955' & 
      first_speaker %in% c('bureaucrat', 'minister', 'opposition'))) + 
  geom_bar(aes(x=factor(fmt_period), fill = committee.type, 
               group=interaction(committee.type, fmt_period), y=avg.speaking), stat = 'identity', position = 'dodge') +
  facet_wrap(~fmt_first_speaker) +
  scale_fill_grey(start = 0, end = 0.8) + theme_bw() + labs(fill = 'Committee Type:') +
  xlab('Time Period') + ylab('Average Proportion of Speeches') +
  theme(legend.position = 'bottom')


# Figure A.6 (b)
g.type.pr.da <- ggplot(da.plot.by.type %>% filter(period != 'Pre-1955' & second_speaker %in% c('bureaucrat', 'minister', 'opposition'))) + 
  geom_bar(aes(x=factor(fmt_period), fill = committee.type, 
               group=interaction(committee.type, fmt_period), y=avg.speaking), stat = 'identity', position = 'dodge') +
  facet_wrap(~fmt_second_speaker) + theme_bw() +
  scale_fill_grey(start = 0, end = 0.8) + xlab('Committee Type') +
  ylab('Average Proportion of Responses') + labs(fill = 'Committee Type:') +
  theme(legend.position = 'bottom')

g.type.vis <- ggpubr::ggarrange(g.type.pr + ggtitle('(a) Activity of Groups'), 
  g.type.pr.da + ggtitle('(b) Discursive Accountability'),
  nrow = 1, common.legend = TRUE, legend = 'bottom')

ggsave(filename = 'figures/fig_a6_descriptive_type.pdf',
       plot = g.type.vis, width = 8, height = 8.5/2)
# ggsave(filename = 'figures/fig_a6b_descriptive_da_type.pdf', 
#        plot = g.type.pr.da, width = 8, height = 6)
# ggsave(filename = 'figures/fig_a6a_descriptive_act_type.pdf', 
#        plot = g.type.pr, width = 8, height = 6)
# 



# Figure A.14
g_da_gov_HOR <- ggplot(discursive.regressions %>% filter(fmt.chamber == 'House of Representatives') %>%
                         filter(first_speaker == 'gov.backbench'  & !grepl(fmt.name, pattern='Intercept')),
                       aes(x = fmt.name, y= est, ymin = est-1.96*se, ymax=est+1.96*se, group=fmt.chamber)) +
  theme.plot + facet_wrap(. ~ fmt_second_speaker, nrow = 2) +
  ylim(c(-0.35, 0.35)) + ggtitle('(a) House of Representatives')

g_da_gov_HOC <- ggplot(discursive.regressions %>% filter(fmt.chamber == 'House of Councillors') %>%
                         filter(first_speaker == 'gov.backbench'  & !grepl(fmt.name, pattern='Intercept')),
                       aes(x = fmt.name, y= est, ymin = est-1.96*se, ymax=est+1.96*se, group=fmt.chamber)) +
  theme.plot + facet_wrap(. ~ fmt_second_speaker, nrow = 2) +
  ylim(c(-0.35, 0.35)) + ggtitle('(b) House of Councillors')

ggsave(filename = 'figures/fig_a14_FE_da_govbb.pdf', ggpubr::ggarrange(g_da_gov_HOR, g_da_gov_HOC, nrow = 1), width = 8.5, height = 8.5/2)



# An extra side-by-side plot of the two chambers
comp_ch_da <- ggplot(discursive.regressions %>%
         filter(first_speaker == 'opposition'  & !grepl(fmt.name, pattern='Intercept')),
       aes(x = fmt.name, y= est, ymin = est-1.96*se, ymax=est+1.96*se, 
           col=as.character(fmt.chamber), group=fmt.chamber)) +
  theme.plot + facet_wrap(. ~ fmt_second_speaker, nrow = 2) +
  ylim(c(-0.35, 0.35)) + ggtitle('(b) Discursive Accountability') +
  labs(col = 'Chamber')

comp_ch_pr <- ggplot(activity.regressions, 
       aes(x=fmt.name,y=est,ymin=est-1.96*se,ymax=est+1.96*se, 
           group=fmt.chamber, col=as.character(fmt.chamber))
) + theme.plot + facet_wrap(. ~ fmt_first_speaker, nrow = 2) +
  ylim(c(-0.15, .15)) + ggtitle('(a) Activity of Groups') +
  labs(col = 'Chamber')
ggpubr::ggarrange(comp_ch_pr, comp_ch_da, nrow = 1)


# Figure 3
# limit.scheme = FALSE, split apart R2 into R2, DPJ, post-DPJ
dpj.markov <- collapse.data.by.scheme(data = store.markov,
  outcome = count,
  scheme.id = 1, limit.scheme = FALSE)

# Same as main code
dpj.activity.data  <- dpj.markov %>% group_by(chamber, committee, cabinet, cabinet_number, 
  budget, period, committee.type, first_speaker) %>% 
  #Get total number of speeches by first_speaker.
  summarize(value = sum(value), total.committee = unique(total.committee)) %>%
  #Get proportion of activity by type.
  mutate(pr.activity = value/total.committee)
dpj.activity.data <- dpj.activity.data %>% filter(first_speaker %in% spkrs)

dpj.activity.regressions <- dpj.activity.data %>% filter(period != 'Pre-1955') %>% group_by(chamber, first_speaker) %>% 
  do(
    fmt.regression(
      plm(formula = pr.activity ~ period, data = ., index = 'committee'), 
      se.type = 'robust')
  )
dpj.discursive.data  <- dpj.markov %>% filter(first_speaker %in% spkrs & second_speaker %in% spkrs)
dpj.discursive.data <- dpj.discursive.data %>% mutate(pr.discursive = value/total)

dpj.discursive.regressions <- dpj.discursive.data %>% filter(period != 'Pre-1955') %>% group_by(chamber, first_speaker, second_speaker) %>% 
  do(
    fmt.regression(
      plm(formula = pr.discursive ~ (period), data = ., index = 'committee'), 
      se.type = 'robust')
  )

dpj.activity.regressions <- clean.columns(dpj.activity.regressions)
dpj.discursive.regressions <- clean.columns(dpj.discursive.regressions)

g_dpj_FE_reg_pr <- ggplot(dpj.activity.regressions %>%
                            filter(fmt.chamber == 'House of Representatives') %>%
                            filter(first_speaker %in% c('junior.minister', 'bureaucrat', 'minister', 'opposition')),
                          aes(x=fmt.name,y=est,ymin=est-1.96*se,ymax=est+1.96*se)) + 
  theme.plot + facet_wrap(. ~ fmt_first_speaker, nrow = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(c(-0.3, 0.3)) +
  ggtitle('(a) Activity of Groups')

g_dpj_da_FE_reg_pr <- ggplot(dpj.discursive.regressions %>% 
                               filter(fmt.chamber == 'House of Representatives') %>%
                               filter(second_speaker %in% c('junior.minister', 'bureaucrat', 'minister', 'opposition')) %>%
                               filter(first_speaker == 'opposition'  & !grepl(fmt.name, pattern='Intercept')), 
                             aes(x = fmt.name, y= est, ymin = est-1.96*se, ymax=est+1.96*se)) +
  theme.plot + facet_wrap(. ~ fmt_second_speaker, nrow = 2) +
  ylim(c(-0.6, 0.6)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('(b) Discursive Accountability')

g_dpj_combined <- ggpubr::ggarrange(g_dpj_FE_reg_pr, g_dpj_da_FE_reg_pr, legend = 'bottom',
                                    common.legend = T, nrow = 1)
ggsave('figures/fig_3_FE_dpj_comb.pdf', g_dpj_combined, width = 8.5, height = 8.5/1.5)

