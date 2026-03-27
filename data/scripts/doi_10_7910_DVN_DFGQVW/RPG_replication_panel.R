rm(list=ls())
source('RPG_replication_aux_functions.R')

mp_level <- readRDS('./final_data/panel_mp_markov.RDS')

#Add period 1 mapping for comparability with main paper
period.mapping <- periodize.data(scheme.id = 1, limit.scheme = T)
mp_level$period <- period.mapping$period$period[match(mp_level$cabinet_number, period.mapping$period$cabinet_number)]
mp_level$period <- match(mp_level$period, period.mapping$labels)

#Get the total # of speeches for each person/name by cabinet/committee
mp_level <- mp_level %>% group_by(period, chamber, cabinet, cabinet_number, committee, 
    pid, ds.name, ds.party, speaker.type, first_speaker, second_speaker) %>%
  summarize(count = sum(count))
#Remove people where there are not matched to Reed-Smith
mp_level <- mp_level %>% filter(!is.na(ds.name))
#Remove people who *are* in Reed-Smith but are bureaucrats (very rare)
print(mean(mp_level$first_speaker == 'bureaucrat'))
mp_level <- mp_level %>% filter(!(first_speaker %in% c('bureaucrat')))
print(table(mp_level$first_speaker))

#Group by MP/cabinet/committee/role and get the number of speeches
discursive_mp <- mp_level %>% group_by(period, chamber, cabinet, cabinet_number, committee, pid, 
  ds.name, ds.party, first_speaker, second_speaker) %>%
  summarize(count = sum(count))
#Add the relevant structural 0s, i.e. if only ministers followed X, need to add "0" for other roles.
discursive_mp <- discursive_mp %>% reshape2::dcast(first_speaker + cabinet_number + period + chamber + cabinet + committee + pid + ds.name + ds.party ~ second_speaker, value.var = 'count', fill = 0)
discursive_mp <- discursive_mp %>% reshape2::melt(id = c('first_speaker', 'period', 'cabinet_number', 'chamber', 'cabinet', 'committee', 'pid', 'ds.name', 'ds.party'))
discursive_mp <- discursive_mp %>% group_by(first_speaker, period, cabinet_number, chamber, cabinet, committee,
  pid, ds.name,  ds.party) %>% mutate(total = sum(value))
#Add a joint ID that is numeric (helps PLM run...)
discursive_mp <- discursive_mp %>% mutate(joint_id = paste(first_speaker, pid, ds.name, ds.party))
uid <- unique(discursive_mp$joint_id)
discursive_mp$nid <- match(discursive_mp$joint_id, uid)
discursive_mp <- discursive_mp %>% arrange(nid)
discursive_mp <- discursive_mp %>% mutate(pr = value/total)
#discursive_mp <- left_join(discursive_mp, cabinet_lookup, by = 'cabinet')
discursive_mp <- discursive_mp %>% mutate(nid_com = paste(nid, chamber, committee, sep ='-'))

#How many periods do various PEOPLE/ROLE/COMMITTEE appear
fe_check <- discursive_mp %>% filter(period != 1) %>%
  group_by(nid, first_speaker, chamber, committee) %>% 
  summarize(n_periods = length(unique(period)))
table(fe_check$n_periods)

discursive_mp <- discursive_mp %>% rename(second_speaker = variable)

repeated_people <- discursive_mp %>% filter(period != 1) %>%
  group_by(nid, first_speaker, committee, chamber) %>% 
  mutate(n_periods = length(unique(period))) %>%
  filter(n_periods > 1) 

#How many people-committee pairs do we have for OPPOSITION
#noted in SI
repeated_people %>% group_by(chamber, first_speaker) %>%
  summarize(l = length(unique(paste(nid, committee, chamber)))) %>%
  reshape2::dcast(first_speaker ~ chamber, value.var = 'l') %>%
  print

repeated_FE <- repeated_people %>% filter(period != 1) %>%
  group_by(first_speaker, chamber, second_speaker) %>%
  do(
    fmt.regression(
      plm(formula = pr ~ factor(period), data = ., index = 'nid_com'), 
      se.type = 'robust')
  )

all_FE <- discursive_mp %>% filter(period != 1) %>%
  group_by(first_speaker, chamber, second_speaker) %>%
  do(
    fmt.regression(
      plm(formula = pr ~ factor(period), data = ., index = 'nid_com'), 
      se.type = 'robust')
  )

all_FE <- all_FE %>% clean.columns()
repeated_FE <- repeated_FE %>% clean.columns()

#Point estimates are identical 
stopifnot(all.equal(all_FE$est, repeated_FE$est))
#SE are identical
stopifnot(all.equal(all_FE$se, repeated_FE$se))

all_FE$man_var <- factor(ifelse(grepl(all_FE$variable, pattern='3'), 'R1', 'R2'), levels = c('R1', 'R2'))

g_opp_mp <- ggplot(all_FE %>% 
         filter(first_speaker == 'opposition' & fmt_second_speaker != 'NA'),
       aes(x=man_var,y=est,ymin=est-1.96*se,ymax=est+1.96*se,
           pch=fmt.chamber,group=fmt.chamber, col = fmt.chamber)) +
  geom_hline(aes(yintercept=0), col = 'black', linetype = 'dashed') +
  geom_errorbar(position = position_dodge(1)) +
  geom_point(position = position_dodge(1)) +
  facet_grid(~ fmt_second_speaker)  +
  theme_bw(base_size = 12) +
  scale_color_manual(values = c('black', 'red')) +
  theme(legend.position = 'bottom') + labs(col = 'Chamber:', pch = 'Chamber:') +
  xlab('Time Period') + ylab('Estimate')

ggsave(filename = 'figures/fig_a13_robustness_mp.pdf', g_opp_mp, width = 8, height = 8.5/2)

