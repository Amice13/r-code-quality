rm(list=ls())
#Data on who represents the government

source('RPG_replication_aux_functions.R')
gov_data <- readRDS('final_data/duo_participation.RDS')

gov_data$committee.type <- factor(gov_data$committee.type, levels = c('Budget', 'Standing', 'Other'))

gov_data <- reshape2::melt(
  gov_data, measure = c('neither', 'both', 'bureaucrat_only', 'minister_only')
) %>% rename(duo_type = variable)

period.mapping <- periodize.data(scheme.id = 1, limit.scheme = T)

gov_data$period <- period.mapping$period$period[match(gov_data$cabinet_number, period.mapping$period$cabinet_number)]
gov_data$period <- factor(gov_data$period, levels = period.mapping$labels, labels = period.mapping$labels)

gov_data <- gov_data %>% group_by(committee, 
    chamber, cabinet, cabinet_number, budget, period, committee.type,
    type, duo_type) %>%
  summarize(count = sum( value ))
#Propogate through "structural" zeros.
gov_data <- dcast(gov_data, formula = ... ~ duo_type, value.var = 'count', fill = 0)
gov_data <- reshape2::melt(gov_data, 
    measure = c('neither', 'both', 'bureaucrat_only', 'minister_only'))
gov_data <- gov_data %>% rename(duo_type = variable)

gov_data <- gov_data %>%
  group_by(committee, chamber, cabinet, cabinet_number, type) %>%
  mutate(total_speeches = sum(value)) %>% ungroup

print(table((gov_data %>% 
               group_by(committee,chamber, type,cabinet_number) %>% 
               summarize(sum = sum(value/total_speeches)))$sum))

activity.duo  <- gov_data %>% group_by(chamber, committee, cabinet, 
      cabinet_number, 
      budget, period, committee.type, type, duo_type) %>% 
  mutate(pr.activity = value/total_speeches)

reg.duo <- activity.duo %>% filter(period != 'Pre-1955') %>% 
  group_by(chamber, type, duo_type) %>% 
  do(
    fmt.regression(
      plm(formula = pr.activity ~ period, data = ., index = 'committee'), 
      se.type = 'robust')
  )


ggplot(reg.duo) +
  geom_point(aes(x=variable,y=est,group=chamber, col = chamber),
             position = position_dodge(width = 1)) + 
  geom_errorbar(aes(x=variable,ymin=est-1.96*se,col=chamber,ymax=est+1.96*se,group=chamber),
             position = position_dodge(width = 1)) + 
  facet_grid(type ~ duo_type) + theme_bw() +
  geom_hline(aes(yintercept=0)) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(
  activity.duo %>% filter(period != 'Pre-1955') %>%
  group_by(committee.type, period, type, duo_type) %>%
  summarize(pr = sum(value)/sum(total_speeches))
) + geom_bar(aes(x=period,y=pr),stat ='identity') +
  facet_grid(type ~ duo_type)


activity.duo %>% filter(period != 'Pre-1955') %>%
  filter(committee.type != 'Budget') %>%
  group_by(type, period, duo_type) %>%
  summarize(pr = sum(value)/sum(total_speeches)) %>%
  dcast(type + period ~ duo_type, value.var = 'pr')
